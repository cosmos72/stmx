;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013 Massimiliano Ghilardi
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

;;;; ** Composing

(define-condition orelse-error (control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to use ~A outside an ~A block" 'orelse 'atomic))))

(defun run-orelse-once (func log)
  "Run FUNC once inside a nested transaction.

If FUNC returns normally, return (values nil (multiple-value-list (funcall func)))
If FUNC calls (retry), return 'wants-to-retry
If FUNC calls (rerun) - used internally by orelse - return 'wants-to-rerun"

  (declare (type function func)
           (type tlog log))

  (prog ((me (log:make-logger)))
     (handler-bind ((retry-error
                     (lambda (err)
                       (declare (ignore err))
                       (log:debug me "Tlog ~A {~A} wants to retry, trying next one"
                                  (~ log) (~ func))
                       (go wants-to-retry)))
                    
                    (rerun-error
                     (lambda (err)
                       (declare (ignore err))
                       (log:debug me "Tlog ~A {~A} wants to re-run, trying next one"
                                  (~ log) (~ func))
                       (go wants-to-rerun)))

                    (condition
                     (lambda (err)
                       (log:trace me "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                                  (~ log) (~ func) (type-of err) (~ err))
                       (if (eq t (locked-valid? log))
                           ;; return normally from lambda to propagate the error
                           (log:debug me "Tlog ~A {~A} will rollback and signal ~A"
                                      (~ log) (~ func) (type-of err))
                           (progn
                             (log:debug me "Tlog ~A {~A} is invalid or unknown, masking the ~A it signalled and trying next one"
                                        (~ log) (~ func) (type-of err))
                             (go wants-to-rerun))))))
    
       (let ((vals (multiple-value-list (run-once func log)))
             (parent-log (tlog-parent log)))
       
         (when (invalid? parent-log)
           (log:debug me "Parent tlog ~A is invalid, re-running it"
                      (~ parent-log))
           (rerun))

         (when (shallow-invalid? log)
           (log:debug me "Tlog ~A {~A} is invalid, trying next one"
                      (~ log) (~ func))
           (go wants-to-rerun))

         (return (values nil vals))))

     wants-to-retry
     (return 'wants-to-retry)

     wants-to-rerun
     (return 'wants-to-rerun)))

       
       

(defun run-orelse-1 (func)
  (declare (type function func))

  (prog* ((parent-log (current-tlog))
          log retry?)
       
     (unless parent-log
       (error 'orelse-error))

     start
     (setf log (new-or-clear-tlog log :parent parent-log))

     (multiple-value-bind (flags vals) (run-orelse-once func log)
       (cond
         ((eq flags 'wants-to-rerun)
          (setf retry? nil)
          (go retry-or-rerun))

         ((eq flags 'wants-to-retry)
          (setf retry? t)
          (go retry-or-rerun))

         (t
          (commit-nested log)
          (log:debug "Tlog ~A {~A} committed to parent tlog ~A"
                     (~ log) (~ func) (~ parent-log))
          (free-tlog log)
          (return-from run-orelse-1 (values-list vals)))))


     retry-or-rerun
     (when *yield-before-rerun*
       (thread-yield))

     (when (invalid? parent-log)
       (log:debug "Parent tlog ~A is invalid, re-running it"
                  (~ parent-log))
       (rerun))

     (when retry?
       (log:debug "Tlog ~A {~A} will sleep, then re-run" (~ log) (~ func))
       ;; wait-tlog only sleeps if log is valid
       (wait-tlog log)
       
       (when (invalid? parent-log)
         (log:debug "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (log:debug "Tlog ~A {~A} re-running now" (~ log) (~ func))
       (go start))
         
     (log:debug "Tlog ~A {~A} was found invalid, re-running it" (~ log) (~ func))
     (go start)))

     



(defun run-orelse-2 (func1 func2)
  "Implementation of RUN-ORELSE for 2 functions."
  (declare (type function func1 func2))

  (prog ((parent-log (current-tlog))
         log1 log2 retry1? retry2?)

     (unless parent-log
       (error 'orelse-error))

     run-tx1
     (setf log1 (new-or-clear-tlog log1 :parent parent-log))

     (multiple-value-bind (flags vals) (run-orelse-once func1 log1)
       (cond
         ((eq flags 'wants-to-retry)
          (setf retry1? t)
          (go run-tx2))

         ((eq flags 'wants-to-rerun)
          (setf retry1? nil)
          (go run-tx2))

         (t
          (commit-nested log1)
          (log:debug "Tlog ~A {~A} committed to parent tlog ~A"
                     (~ log1) (~ func1) (~ parent-log))
          (free-tlog log1)
          (when log2 (free-tlog log2))
          (return-from run-orelse-2 (values-list vals)))))


     run-tx2
     (setf log2 (new-or-clear-tlog log2 :parent parent-log))

     (multiple-value-bind (flags vals) (run-orelse-once func2 log2)
       (cond
         ((eq flags 'wants-to-retry)
          (setf retry2? t)
          (go retry-or-rerun))

         ((eq flags 'wants-to-rerun)
          (setf retry2? nil)
          (go retry-or-rerun))

         (t
          (commit-nested log2)
          (log:debug "Tlog ~A {~A} committed to parent tlog ~A"
                     (~ log2) (~ func2) (~ parent-log))
          (free-tlog log1)
          (free-tlog log2)
          (return-from run-orelse-2 (values-list vals)))))

     retry-or-rerun
     (when *yield-before-rerun*
       (thread-yield))

     (when (invalid? parent-log)
       (log:debug "Parent tlog ~A is invalid, re-running it"
                  (~ parent-log))
       (rerun))

     (when (or (not retry1?) (invalid? log1))
       ;; func1 wants to rerun, or wants to retry but is invalid.
       ;; in both cases, rerun it
       (go run-tx1))

     (when (or (not retry2?) (invalid? log2))
       ;; func2 wants to rerun, or wants to retry but is invalid.
       ;; in both cases, rerun it
       (go run-tx2))

     ;; here all txs want to retry, and were valid when we checked.
     ;; Note: it does not mean they are compatible and still valid

     (unless (merge-tlog-reads log1 log2)
       ;; some tlog was incompatible with previous ones
       (log:debug "Tlog ~A {~A} is incompatible with tlog ~A {~A}, rerunning ORELSE"
                      (~ log1) (~ func1) (~ log2) (~ func2))
       (go run-tx1))

     (log:debug "ORELSE will sleep, then re-run")
     ;; wait-tlog only sleeps if log is valid
     (wait-tlog log1)
       
     (when (invalid? parent-log)
       (log:debug "Parent tlog ~A is invalid, re-running it"
                  (~ parent-log))
       (rerun))

     (log:debug "Re-running ORELSE")
     (go run-tx1)))









(defstruct orelse-tx
  (func  nil :type function)
  (log   nil :type (or null tlog))
  (retry  nil :type boolean))


(defun free-tx-logs (txs)
  "Return to TLOG pool all logs contained in TXS"
  (loop for tx across txs
     while tx
     for log = (orelse-tx-log tx)
     while log do
       (free-tlog log)))


(defun find-first-rerun-or-invalid-tx (txs me)
  "Return index of the first tx that wants to rerun or is invalid.
Return nil if all tx are valid and want to retry."
  (declare (type simple-vector txs))

  (loop for i from 0 to (1- (length txs))
     for itx    = (svref txs i)
     for ifunc  = (orelse-tx-func  itx)
     for ilog   = (orelse-tx-log   itx)
     for iretry = (orelse-tx-retry itx)
     do
       (when (or (not iretry) (shallow-invalid? ilog))
         ;; itx wants to rerun, or wants to retry but is invalid
         (log:debug me "Tlog ~A {~A} ~A invalid, re-running it"
                    (~ ilog) (~ ifunc) (if iretry "is" "was found"))
         (return-from find-first-rerun-or-invalid-tx i)))
  nil)


(defun merge-tlog-reads-tx (txs me)
  "Return merged tlog of all tx, or nil if some tlog are mutually incompatible."
  (declare (type simple-vector txs))

  (let1 log (orelse-tx-log (svref txs 0))
    
    (loop for i from 1 to (1- (length txs))
       for itx    = (svref txs i)
       for ifunc  = (orelse-tx-func  itx)
       for ilog   = (orelse-tx-log   itx)
       do
         (unless (merge-tlog-reads log ilog)
           (log:debug me "Tlog ~A {~A} is incompatible with previous ones, rerunning ORELSE"
                      (~ ilog) (~ ifunc))
           (return-from merge-tlog-reads-tx nil)))
    log))
        

(defun run-orelse-n (&rest funcs)
  "Implementation of RUN-ORELSE for N functions."
  (declare (type list funcs))


  ;; n-ary implementation, n transactions tx[0...n-1]: set i=0, run tx[i], then:
  ;;
  ;; a) if tx[i] is executed:
  ;;    1. if tx[i] is invalid:
  ;;       1.1 if tx[i+1] exists, run it. reason: more possibilities
  ;;           to get a valid run of one of tx[]
  ;;       1.2 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;           this is necessary to break infinite loops cycling
  ;;           on some of the tx[i], caused by the parent-tx being invalid
  ;;       1.3 if i<>0 and tx[0] is invalid, run it. reason: it means some TVARs
  ;;           relevant for tx[0] have changed, and there is no reason
  ;;           to re-execute parent-tx
  ;;       1.4 run tx[i] again
  ;;    2. if tx[i] wants to retry:
  ;;       2.1. if tx[i+1] exists, run it. reason: orelse exists exactly
  ;;            to implement this behaviour
  ;;       2.2. otherwise, sleep on tx[0]+ ... + tx[n-1]
  ;;    3. if tx[i] wants to commit (returns normally), merge to parent log and return
  ;;    4. if tx[i] wants to rollback (signals an error), rollback and signal the error
  ;; 
  ;; c) if sleeping on tx[0]+ ... + tx[n-1]:
  ;;    1. if tx[0] ... tx[n-1] are incompatible => at least one of them is invalid
  ;;       1.1 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       1.2 otherwise, run the first invalid one among tx[0] ... tx[n-1]
  ;;    2. merge tx[0] ... tx[n-1] logs into logN, and sleep on it. when waking up:
  ;;       2.1 if logN is valid, sleep again on it
  ;;       2.2 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       2.3 run tx[0] (we could check which tx is invalid to run it instead,
  ;;           but we lost some logs when merging them)


  (let ((parent-log (current-tlog))
        txs tx index)

    (unless parent-log
      (error 'orelse-error))

    (unless funcs
      (return-from run-orelse-n (values)))

    (setf txs (make-array (list (length funcs))
                          :element-type '(or null orelse-tx)
                          :initial-element nil))

    (labels ((ensure-tx ()
               (setf tx (svref txs index))
               (unless tx
                 (setf tx (make-orelse-tx :func (pop funcs)))
                 (setf (svref txs index) tx))
               tx)
             
             (set-index (idx)
               (setf index idx)
               (ensure-tx))

             (wants-to-retry ()
               (setf (orelse-tx-retry tx) t))

             (wants-to-rerun ()
               (setf (orelse-tx-retry tx) nil)))
           

      (prog (func log (me (log:make-logger)))

       start
       (set-index 0)
       (go run-tx)


       run-tx
       (setf func (orelse-tx-func tx)
             log  (new-or-clear-tlog (orelse-tx-log tx) :parent parent-log)
             (orelse-tx-log tx) log)

       (multiple-value-bind (flags vals) (run-orelse-once func log)
         (cond
           ((eq flags 'wants-to-retry)
            (wants-to-retry)
            (go run-next))

           ((eq flags 'wants-to-rerun)
            (wants-to-rerun)
            (go run-next))

           (t
            (commit-nested log)
            (log:debug me "Tlog ~A {~A} committed to parent tlog ~A"
                       (~ log) (~ func) (~ parent-log))
            (free-tx-logs txs)
            (return-from run-orelse-n (values-list vals)))))


       run-next
       (when (< (incf index) (length txs))
         (ensure-tx)
         (go run-tx))
       (go retry-or-rerun)


       retry-or-rerun
       (when *yield-before-rerun*
         (thread-yield))

       (when (invalid? parent-log)
         (log:debug me "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (let1 idx (find-first-rerun-or-invalid-tx txs me)
         (when idx
           ;; (svref txs idx) wants to rerun, or wants to retry but is invalid.
           ;; in both cases, rerun it
           (set-index idx)
           (go run-tx)))


       ;; here all txs want to retry, and were valid when we checked.
       ;; Note: it does not mean they are compatible and still valid

       (setf log (merge-tlog-reads-tx txs me))
       (unless log
         ;; some tlog was incompatible with previous ones
         (go start))
           
       (log:debug me "ORELSE will sleep, then re-run")
       ;; wait-tlog only sleeps if log is valid
       (wait-tlog log)
       
       (when (invalid? parent-log)
         (log:debug me "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (log:debug me "Re-running ORELSE")
       (go start)))))



(defun run-orelse (&rest funcs)
  "Function variant of `orelse'. Execute the functions in FUNCS list
one by one from left to right in separate, nested transactions until one succeeds
\(i.e. commits) or signals an error.

If a nested transaction is invalid or wants to retry, run the next one.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  (declare (type list funcs))
  (let ((f1 (first funcs))
        (f2 (second funcs)))
    (cond
      ((null f1)            (values))
      ((null f2)            (run-orelse-1 f1))
      ((null (third funcs)) (run-orelse-2 f1 f2))
      (t                    (apply #'run-orelse-n funcs)))))
         


(defmacro orelse (&body body)
  "Execute each form in BODY from left to right in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."

  (let* ((funcs (loop for form in body collect
                     `(lambda () ,form)))
         (f1 (first funcs))
         (f2 (second funcs)))
    (cond
      ((null f1)            `(values))
      ((null f2)            `(run-orelse-1 ,f1))
      ((null (third funcs)) `(run-orelse-2 ,f1 ,f2))
      (t                    `(run-orelse-n ,@funcs)))))
         
      



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

  `(orelse (multiple-value-call #'values t (progn ,@body))
           nil))
