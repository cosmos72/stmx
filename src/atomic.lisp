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

(define-condition retry-error (control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'retry 'atomic))))


(define-condition rerun-error (control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'rerun 'orelse)))
  (:documentation "Signalled by ORELSE to force re-running the parent transaction."))




(defun retry ()
  "Abort the current transaction and re-run it again from the beginning.

Before re-executing, the transaction will wait on all variables that it read
until at least one of them changes."
  (error 'retry-error))


(defun rerun ()
  "Abort the current transaction and immediately re-run it from the
beginning without waiting. Used by ORELSE."
  (error 'rerun-error))

  


;;;; ** Running


(defmacro atomic (&rest body)
  "Main entry point for STMX.

Run BODY in a memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

A memory transaction can also retry: in such case ATOMIC will abort it,
wait until some of the value read by the transaction have changed,
then re-run the transaction from the beginning.

Since STMX transactions do not lock memory, it is possible for different
transactions to try to update the same memory (almost) simultaneously.
In such case, the conflict is detected when they try to commit,
and only one conflicting transaction is allowed to commit:
all others are immediately re-rund again from the beginning.

For this reason, a transaction ABSOLUTELY MUST NOT perform any irreversible
operation such as INPUT/OUTPUT: the result would be that I/O is executed
multiple times, or executed even when it should not!
Irreversible operations MUST be performed OUTSIDE transactions,
for example by queueing them into transactional memory that another thread
will consume and then, OUTSIDE transactions, actually perform them.

For how create transactional memory, see TRANSACTIONAL or TVAR.
For another way to run transactions, see also TRANSACTION.
For advanced features inside transactions, see RETRY, ORELSE and NONBLOCKING.

For pre-defined transactional classes, see the package STMX.UTIL"

  (let1 id (when (eq :id (first body))
             (pop body)
             (pop body))
    `(run-atomic (lambda () ,@body) :id ,id)))



(defun run-once (tx log)
  "Internal function invoked by RUN-ATOMIC and RUN-ORELSE.

Run once the function TX inside a transaction,
using LOG as its transaction log."
  (declare (type function tx)
           (type tlog log))

  (with-recording-to-tlog log
    (prog (retry? err vals (parent (parent-of log)))

     run
     (log:debug "Tlog ~A {~A} starting~A" (~ log) (~ tx)
                (if parent
                    (format nil ", parent is tlog ~A" (~ parent))
                    ""))
     (handler-case
         (progn
           (setf vals (multiple-value-list (funcall tx)))
           (log:trace "Tlog ~A {~A} wants to commit, returned: ~{~A ~}"
                      (~ log) (~ tx) vals))

       (rerun-error ()
         (log:trace "Tlog ~A {~A} wants to rerun" (~ log) (~ tx))
         (clear-tlog log :parent parent)
         (setf vals nil) ;; paranoia
         (go run))
         
       (retry-error ()
         (setf retry? t)
         (log:trace "Tlog ~A {~A} wants to retry" (~ log) (~ tx)))

       (t (cond)
         (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                    (~ log) (~ tx) (type-of cond) (~ cond))
         (setf err cond)))

     (return-from run-once (values retry? err vals)))))



(defun run-atomic (tx &key id)
  "Function equivalent of the ATOMIC macro.

Run the function TX inside a transaction.
Commit if it returns normally, rollback if it signals an error.

If the transaction is invalid (conflicts), re-run it immediately.
If the transaction retries, re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  (when (and (recording?) (current-tlog))
    (return-from run-atomic (funcall tx)))

  (when id
    (setf (~ tx) id))

  (prog ((log (new 'tlog)))

   run
   (multiple-value-bind (retry? err vals) (run-once tx log)
     (when (invalid? log)
       (log:debug "Tlog ~A {~A} is invalid, re-running it" (~ log) (~ tx))
       (go re-run))
     (when retry?
       (log:debug "Tlog ~A {~A} will sleep, then retry" (~ log) (~ tx))
       (wait-tlog log)
       (go re-run))
     (when err
       (log:debug "Tlog ~A {~A} will rollback and signal ~A"
                  (~ log) (~ tx) (type-of err))
       (error err))
     (when (commit log)
       (return (values-list vals)))

     (log:debug "Tlog ~A {~A} could not commit, re-running it"
                (~ log) (~ tx))
     (go re-run))
   
   re-run
   (clear-tlog log)
   (go run)))




