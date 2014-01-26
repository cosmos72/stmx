;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :stmx)

(enable-#?-syntax)


;;;; ** Executing multiple transactions as alternatives

(define-condition orelse-error (control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to use ~A outside an ~A block" 'orelse 'atomic))))




(defstruct orelse-tx
  (func  nil :type function)
  (log   nil :type (or null tlog))
  (retry nil :type boolean))


(defun free-tx-logs (txs)
  "Return to TLOG pool all logs contained in TXS"
  (loop for tx across txs
     while tx
     for log = (orelse-tx-log tx)
     while log do
       (free-tlog log)))


(defun find-first-rerun-tx (txs me)
  "Return index of the first tx that wants to rerun or is invalid.
Return nil if all tx are valid and want to retry."
  (declare (type simple-vector txs)
           (ignorable me))

  (loop for i from 0 to (1- (length txs))
     for itx    = (svref txs i)
     for iretry = (orelse-tx-retry itx)
     do
       (unless iretry
         ;; itx wants to rerun
         (log.debug me "Tlog ~A {~A} ~A invalid, re-running it"
                    (~ (orelse-tx-log   itx))
                    (~ (orelse-tx-func  itx))
                    (if iretry "is" "was found"))
         (return-from find-first-rerun-tx i)))
  nil)


(defun merge-tlog-reads-tx (txs me)
  "Return merged tlog of all tx, or nil if some tlog are mutually incompatible."
  (declare (type simple-vector txs)
           (ignorable me))

  (let1 log (orelse-tx-log (svref txs 0))
    
    (loop for i from 1 to (1- (length txs))
       for itx    = (svref txs i)
       for ifunc  = (orelse-tx-func  itx)
       for ilog   = (orelse-tx-log   itx)
       do
         (unless (merge-tlog-reads log ilog)
           (log.debug me "Tlog ~A {~A} is incompatible with previous ones, rerunning ORELSE"
                      (~ ilog) (~ ifunc))
           (return-from merge-tlog-reads-tx nil)))
    log))
        

(defun run-orelse (&rest funcs)
  "Function variant of `orelse'. Execute the functions in FUNCS list
one by one from left to right in separate, nested transactions until one succeeds
\(i.e. commits) or signals an error.

If a nested transaction is invalid or wants to retry, run the next one.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  (declare (type list funcs))

  ;; n-ary implementation, n transactions tx[0...n-1]: set i=0, run tx[i], then:
  ;;
  ;; a. if tx[i] is executed:
  ;;    1. if tx[i] wants to retry or rerun:
  ;;       1.1 if tx[i+1] exists, run it (increase i and jump to a).
  ;;           reason: orelse exists to implement this behaviours
  ;;       1.2 otherwise, if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;           this is necessary to break infinite loops cycling
  ;;           on some of the tx[i], caused by the parent-tx being invalid
  ;;       1.3 otherwise, find the smallest i such that tx[i] wanted to rerun, and run it.
  ;;       1.4 otherwise it means all tx[i] wanted to retry, so jump to b.
  ;;    3. if tx[i] wants to commit (returns normally), merge to parent log and return
  ;;    4. if tx[i] wants to rollback (signals an error), rollback and signal the error
  ;; 
  ;; b. all tx[i] want to retry.
  ;;    1. if tx[0] ... tx[n-1] are incompatible => at least one of them is invalid
  ;;       1.1 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       1.2 otherwise, run the first invalid one among tx[0] ... tx[n-1]
  ;;    2. merge tx[0] ... tx[n-1] logs into logN, and sleep on it. when waking up:
  ;;       2.1 if logN is valid, sleep again on it
  ;;       2.2 otherwise, if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       2.3 otherwise, run tx[0] (set i=0 and jump to a).

  
  ;; ORELSE does not support hardware transactions!
  (when (hw-transaction-supported-and-running?)
    (if funcs
        (hw-transaction-abort)
        (return-from run-orelse (values))))


  (let ((parent-log (aif (current-tlog) it (error 'orelse-error)))
        rerunning
        (index 0)
        tx
        (txs (if funcs
                 (make-array (length funcs)
                             :element-type '(or null orelse-tx)
                             :initial-element nil)
                 (return-from run-orelse (values)))))

    (declare (type boolean rerunning)
             (type fixnum index)
             (type simple-vector txs))

    (labels ((ensure-tx ()
               (unless (setf tx (svref txs index))
                 (setf tx (make-orelse-tx :func (pop funcs)))
                 (setf (svref txs index) tx))
               (setf rerunning nil)
               tx)
             
             (set-index (idx)
               (setf index idx)
               (ensure-tx))

             (wants-to-retry ()
               (global-clock/sw/stat-committed)
               (setf (orelse-tx-retry tx) t))

             (wants-to-rerun ()
               (global-clock/sw/stat-aborted)
               (setf (orelse-tx-retry tx) nil)))
           

      (prog (func log (me (log.make-logger)))

       start
       (set-index 0)
       (go run-tx)


       run-tx
       (log.debug me "ORELSE starting ~:R branch" (1+ index))

       (setf func (orelse-tx-func tx)
             log  (new-or-clear-tlog (orelse-tx-log tx) :parent parent-log)
             (orelse-tx-log tx) log)

       ;; in a nested transaction we must use the same read-version as the parent
       ;; because we inherit its tlog-reads and tlog-writes...
       (setf (tlog-read-version log) (tlog-read-version parent-log))

       (handler-case
           (return-from run-orelse
             (multiple-value-prog1 (run-once func log)

               (global-clock/sw/stat-committed)
               (commit-nested log)
               (log.debug me "Tlog ~A {~A} committed to parent tlog ~A"
                          (~ log) (~ func) (~ parent-log))
               (free-tx-logs txs)))
         
         (retry-error ()
           (log.debug me "Tlog ~A {~A} wants to retry, trying next one"
                      (~ log) (~ func))
           (wants-to-retry)
           (go run-next))

         (rerun-error ()
           (wants-to-rerun)

           #?+global-clock/spurious-failures-in-single-thread
           (if rerunning
               (log.debug me "Tlog ~A {~A} wants to re-run, trying next one"
                          (~ log) (~ func))
               (progn
                 (log.debug me "Tlog ~A {~A} wants to re-run, trying it again before moving to next one"
                            (~ log) (~ func))
                 (go wants-to-rerun-maybe-spurious-failure)))

           (go run-next)))




       #?+global-clock/spurious-failures-in-single-thread
       wants-to-rerun-maybe-spurious-failure

       #?+global-clock/spurious-failures-in-single-thread
       (progn
         ;; since we are validating parent-log, we should update its read-version
         ;; otherwise the nested transactions (which inherit the parent read-version)
         ;; can enter an infinite retry or rerun loop
         (setf (tlog-read-version parent-log) (global-clock/sw/after-abort))

         (when (invalid? parent-log)
           (log.debug me "Parent tlog ~A is invalid, re-running it"
                      (~ parent-log))
           (rerun))

         ;; rerun TX a second time before giving up
         (setf rerunning t)
         (go run-tx))

         

       run-next
       (when (< (incf index) (length txs))
         (ensure-tx)
         (go run-tx))

       ;; since we are validating parent-log, we should update its read-version
       ;; otherwise the nested transactions (which inherit the parent read-version)
       ;; can enter an infinite retry or rerun loop
       (setf (tlog-read-version parent-log) (global-clock/sw/after-abort))

       (when (invalid? parent-log)
         (log.debug me "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (let1 idx (find-first-rerun-tx txs me)
         (when idx
           ;; (svref txs idx) wants to rerun, so rerun it
           (maybe-yield-before-rerun)
           (set-index idx)
           (go run-tx)))
       ;; fallthrough


       retry
       ;; All txs want to retry.
       ;; Note: it does not mean they are compatible

       (setf log (merge-tlog-reads-tx txs me))
       (unless log
         ;; some tlog was incompatible with previous ones
         (maybe-yield-before-rerun)
         (go start))
           
       (log.debug me "ORELSE will sleep, then re-run")
       (wait-tlog log)
       
       ;; since we are validating parent-log, we should update its read-version
       ;; otherwise the nested transactions (which inherit the parent read-version)
       ;; can enter an infinite retry or rerun loop
       (setf (tlog-read-version parent-log) (global-clock/sw/start-read))

       (when (invalid? parent-log)
         (log.debug me "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (log.debug me "Re-running ORELSE")
       (go start)))))
         


(defmacro orelse (&body body)
  "Execute each form in BODY from left to right in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

If a form calls (RETRY) or has a temporary failure \(temporary failures as
for example conflicts are normally invisible because (ATOMIC ...) re-executes
them automatically) advance to the next form and run it instead of retrying
the current one.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."

  (let ((funcs (loop for form in body collect
                    `(lambda () ,form))))
    `(run-orelse ,@funcs)))
         
      



(defun run-nonblocking (func)
  "Function version of NONBLOCKING macro: execute the function FUNC
in a nested transaction and:
a) in case of transaction conflicts, re-execute FUNC
b) if FUNC returns normally, commit and return multiple values:
   T followed by the values returned by FUNC.
b) if FUNC signals an error, rollback and raise such error.
d) if FUNC attempts to retry, immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (declare (type function func))
  (orelse (multiple-value-call #'values t (funcall func))
          nil))



(defmacro nonblocking (&body body)
  "Execute all the forms in BODY in a single nested transaction and:
a) in case of transaction conflicts, re-execute BODY
b) if BODY returns normally, commit and return multiple values:
   T followed by the values returned by BODY.
b) if BODY signals an error, rollback and raise such error.
d) if BODY attempts to retry, immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  `(orelse (multiple-value-call #'values t (locally ,@body))
           nil))
