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

(defstruct orelse-tx
  (func  nil :type function)
  (log   nil :type (or null tlog))
  (retry t   :type boolean))



(defun run-orelse-tx (&rest txs)
  (declare (type list txs))

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
         txs-ref tx any-retry? any-rerun?)
    
    (unless parent-log
      (error 'orelse-error))

    (unless txs
      (return-from run-orelse-tx (values)))

    (flet ((initialize ()
             (setf txs-ref    txs
                   tx         (first txs)
                   any-retry? nil
                   any-rerun? nil))

           (wants-to-rerun ()
             (setf (orelse-tx-retry tx) nil
                   any-rerun? t))
           
           (wants-to-retry ()
             (setf (orelse-tx-retry tx) t
                   any-retry? t)))

      (prog (vals func log (me (log:make-logger)))

       start
       (initialize)
       (go run-tx)


       run-next
       (setf tx (pop txs-ref))
       (if tx
           (go run-tx)
           (go retry-or-rerun))


       run-tx
       (setf func (orelse-tx-func tx)
             log  (make-or-clear-tlog (orelse-tx-log tx) :parent parent-log)
             (orelse-tx-log tx) log)

       (handler-bind ((retry-error
                       (lambda (err)
                         (declare (ignore err))
                         (log:debug me "Tlog ~A {~A} wants to retry, trying next one"
                                    (~ log) (~ func))
                         (wants-to-retry)
                         (go run-next)))
                    
                      (rerun-error
                       (lambda (err)
                         (declare (ignore err))
                         (log:debug me "Tlog ~A {~A} wants to re-run, trying next one"
                                    (~ log) (~ func))
                         (wants-to-rerun)
                         (go run-next)))

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
                               (wants-to-rerun)
                               (go run-next))))))
       
         (setf vals (multiple-value-list (run-once func log)))

         (when (invalid? log)
           (log:debug me "Tlog ~A {~A} is invalid, trying next one"
                      (~ log) (~ func))
           (wants-to-rerun)
           (go run-next))

         (go commit))
   

       commit
       (commit-nested log)
       (log:debug me "Tlog ~A {~A} committed: merged with parent tlog ~A"
                  (~ log) (~ tx) (~ parent-log))
       (go done)
   

       done
       (return-from run-orelse-tx (values-list vals))


       retry-or-rerun
       (if any-rerun?
           (go rerun)
           (go retry))


       rerun
       (when (invalid? parent-log)
         (log:debug me "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (initialize)
       (loop for itx = (pop txs-ref)
          while itx
          for ilog = (orelse-tx-log itx)
          when (invalid? ilog)
          do
            (log:debug me "Tlog ~A {~A} is invalid, re-running it"
                       (~ ilog) (~ (orelse-tx-func itx)))
            (setf tx itx)
            (go run-tx))
       (go retry)


       retry
       (initialize)
       (setf tx (pop txs-ref))
       (setf log (orelse-tx-log tx))
       
       (loop for itx in txs-ref
          for ilog = (orelse-tx-log itx)
          unless
            (merge-reads-of log ilog)
          do
            (log:debug me "Tlog ~A {~A} is incompatible with the previous ones, re-running the whole ORELSE"
                       (~ ilog) (~ (orelse-tx-func itx)))
            (go start))

       (log:debug me "Sleeping for tlogs ~{~A ~}"
                  (loop for itx in txs
                     for ilog = (orelse-tx-log itx)
                     collect (~ ilog)))
       ;; wait-tlog only sleeps if log is invalid
       (wait-tlog log)
       (go start)))))





(defun run-orelse (&rest funcs)
  "Function variant of `orelse'. Execute the functions in FUNCS list one by one
from left to right in separate, nested transactions until one succeeds
\(i.e. commits) or signals an error.

If a nested transaction is invalid or wants to retry, run the next one.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  (declare (type list funcs))

  (apply #'run-orelse-tx
         (loop for func in funcs collect
              (make-orelse-tx :func func))))
         


(defmacro orelse (&body body)
  "Execute each form in BODY from left to right in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."

  (if body
      (let1 txs
          (loop for form in body collect
               `(make-orelse-tx :func (lambda () ,form)))
        `(run-orelse-tx ,@txs))
      `(values)))
      



(defun run-nonblocking (tx &key id)
  "Function version of NONBLOCKING macro: execute the function tx in a nested transaction and:
a) if the nested transaction succeeds (i.e. commits), return multiple values:
   T followed by the values returned by the transaction.
b) if the nested transaction signals an error, raise such error.
c) if the nested transaction attempts to retry,
   immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (declare (type function tx))
  (orelse2 (multiple-value-call #'values t (funcall tx))
           nil
           :id1 (or id 'nonblocking)
           :id2 'nonblocking-nil))


(defmacro nonblocking (&body body)
  "Execute all the form in BODY in a single nested transaction and:
a) if the nested transaction succeeds (i.e. commits), return multiple values:
   T followed by the values returned by the transaction.
b) if the nested transaction signals an error, raise such error.
c) if the nested transaction attempts to retry,
   immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (let1 id (when (eq :id (first body))
             (pop body)
             (pop body))
    `(orelse2 (multiple-value-call #'values t (progn ,@body))
              nil
              :id1 ,(or id ''nonblocking)
              :id2 'nonblocking-nil)))
