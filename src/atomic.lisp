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

;(eval-always
;  (enable-pf-reader))


;;;; ** Defining

(defmacro transactional ((defclass class-name direct-superclasses direct-slots &rest class-options))
  "Define CLASS-NAME as a new transactional class.
Use this macro to wrap a normal DEFCLASS as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))

The effect is the same as DEFCLASS, plus:
- by default, slots are transactional memory (implemented by TVARs)
- it inherits also from TRANSACTIONAL-OBJECT
- the metaclass is TRANSACTIONAL-CLASS"
    `(eval-always
       (,defclass ,class-name ,direct-superclasses
         ,direct-slots
         ,@class-options
         (:metaclass transactional-class))))


(defmacro transaction ((defun-or-defmethod func-name args &body body))
  "Define FUNC-NAME as a new atomic function or method.
Use this macro to wrap a normal DEFUN or DEFMETHOD as follows:
\(TRANSACTION (DEFUN function-name (arguments) body))
or
\(TRANSACTION (DEFMETHOD function-name (arguments) body))

The effect is the same as DEFUN - or DEFMETHOD - plus:
- the BODY is wrapped inside (atomic ...)"
    `(eval-always
       (,defun-or-defmethod ,func-name ,args
         (atomic :id ',func-name
           ,@body))))



;;;; ** Retrying

(define-condition stmx-control-error (control-error)
  ()
  (:documentation "Parent class of all STMX errors"))


(define-condition retry-error (stmx-control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'retry 'atomic)))

  (:documentation "Signalled by RETRY. It is captured by ATOMIC and ORELSE,
so it is visible to application code only in case of bugs"))



(define-condition rerun-error (stmx-control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Transactional memory conflict detected inside ~A.
If you see this error, there is a bug either in STMX or in application code.
Possible reason: code registered with ~A attempted to read transactional memory
not already read or written during the transaction" 'orelse 'after-commit)))

  (:documentation "Signalled by ORELSE to force re-running the parent transaction
in case of conflicts. It is captured by ATOMIC, so it is visible
to application code only in case of bugs"))



(let1 retry-error-obj (make-condition 'retry-error)
  (defun retry ()
    "Abort the current transaction and re-run it again from the beginning.

Before re-executing, the transaction will wait on all variables that it read
until at least one of them changes."
    (error retry-error-obj)))


(let1 rerun-error-obj (make-condition 'rerun-error)
  (defun rerun ()
    "Abort the current transaction and immediately re-run it from the
beginning without waiting. Used by ORELSE."
    (error rerun-error-obj)))

  


;;;; ** Running


(defmacro atomic (&rest body)
  "Main entry point for STMX.

Run BODY in a memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

A memory transaction can also retry: in such case ATOMIC will abort it,
wait until some of the value read by the transaction have changed,
then re-run the transaction from the beginning.

Since STMX transactions do not lock memory, it is possible for different
transactions to try to update the same memory (almost) simultaneously.
In such case, the conflict is detected when they try to commit or rollback,
and only one conflicting transaction is allowed to commit:
all others are immediately re-run again from the beginning,
also ignoring any error they may have signalled.

For this reason, a transaction ABSOLUTELY MUST NOT perform any irreversible
operation such as INPUT/OUTPUT: the result would be that I/O is executed
multiple times, or executed even when it should not!
Irreversible operations MUST be performed OUTSIDE transactions,
for example by queueing them into transactional memory that another thread
will consume and then, OUTSIDE transactions, actually perform them.

For how to create transactional memory, see TRANSACTIONAL or TVAR.
For another way to run transactions, see also TRANSACTION.
For advanced features inside transactions, see RETRY, ORELSE, NONBLOCKING,
    BEFORE-COMMIT and AFTER-COMMIT.

For pre-defined transactional classes, see the package STMX.UTIL"

  (let1 id (when (eq :id (first body))
             (pop body)
             (pop body))
    (if body
        `(run-atomic (lambda () ,@body) :id ,id)
        `(values))))


(declaim (type boolean *yield-before-rerun*))
(defvar *yield-before-rerun* nil)

(defun run-once (tx log)
  "Internal function invoked by RUN-ATOMIC and RUN-ORELSE2.

Run once the function TX inside a transaction,
using LOG as its transaction log."
  (declare (type function tx)
           (type tlog log))

  (with-recording-to-tlog log
    (prog ((parent (parent-of log)))

     run
     (log:debug "Tlog ~A {~A} starting~A" (~ log) (~ tx)
                (if parent
                    (format nil ", parent is tlog ~A" (~ parent))
                    ""))
     (restart-case
         (return (funcall tx))

       (rerun-once ()
         (log:trace "Tlog ~A {~A} will rerun" (~ log) (~ tx))
         (make-or-clear-tlog log :parent parent)
         (when *yield-before-rerun*
           (thread-yield))
         (go run))))))


(defun run-atomic (tx &key id)
  "Function equivalent of the ATOMIC macro.

Run the function TX inside a transaction.
If the transaction is invalid (conflicts) re-run TX immediately, ignoring
any error it may signal.

Otherwise, commit if TX returns normally, or rollback if it signals an error.

Finally, if TX called (retry), re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  (when (and (recording?) (current-tlog))
    (return-from run-atomic (funcall tx)))

  (when id
    (setf (~ tx) id))

  (prog ((log (make-tlog)))

   run
   (handler-bind ((retry-error
                   (lambda (err)
                     (declare (ignore err))
                     (log:trace "Tlog ~A {~A} wants to retry" (~ log) (~ tx))
                     (go retry)))
                  
                  (rerun-error
                   (lambda (err)
                     (declare (ignore err))
                     (log:trace "Tlog ~A {~A} wants to re-run" (~ log) (~ tx))
                     (invoke-restart 'rerun-once)))

                  (condition
                   (lambda (err)
                     (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                                (~ log) (~ tx) (type-of err) (~ err))
                     (if (eq t (locked-valid? log))
                         ;; return normally from lambda to propagate the error
                         (log:debug "Tlog ~A {~A} will rollback and signal ~A"
                                    (~ log) (~ tx) (type-of err))
                         (progn
                           (log:debug "Tlog ~A {~A} is invalid or unknown, masking the ~A it signalled and re-running it"
                                      (~ log) (~ tx) (type-of err))
                           (invoke-restart 'rerun-once))))))
         
     (return
       (multiple-value-prog1
           (run-once tx log)
         (log:trace "Tlog ~A {~A} wants to commit" (~ log) (~ tx))

         ;; commit also checks if log is valid
         (if (commit log)
             ;; all done, prepare to return.
             ;; we are not returning TLOGs to the pool in case tx signaled an error,
             ;; but that's not a problem since the TLOG pool is just a speed optimization
             (free-tlog log)

             (progn
               (log:debug "Tlog ~A {~A} could not commit, re-running it" (~ log) (~ tx))
               (go rerun))))))

   retry
   (log:debug "Tlog ~A {~A} will sleep, then retry" (~ log) (~ tx))
   ;; wait-tlog sleeps only if log is valid
   (wait-tlog log)
   (go rerun)

   rerun
   (make-or-clear-tlog log :parent (parent-of log))
   (go run)))




