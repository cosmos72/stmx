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
  `(,defclass ,class-name ,(ensure-transactional-object-among-superclasses direct-superclasses)
     ,(adjust-transactional-slots-definitions direct-slots class-name direct-superclasses)
     ,@class-options
     #+stmx-must-disable-optimize-slot-access (:optimize-slot-access nil)
     (:metaclass transactional-class)))


(defmacro transaction ((defun-or-defmethod func-name args &body body))
  "Define FUNC-NAME as a new atomic function or method.
Use this macro to wrap a normal DEFUN or DEFMETHOD as follows:
\(TRANSACTION (DEFUN function-name (arguments) body))
or
\(TRANSACTION (DEFMETHOD function-name (arguments) body))

The effect is the same as DEFUN - or DEFMETHOD - plus:
- the BODY is wrapped inside (atomic ...)"
  `(,defun-or-defmethod ,func-name ,args
     ;; move docstring here
     ,@(when (and (stringp (first body)) (rest body))
             (list (pop body)))
     ;; also copy all declarations
     ,@(loop for form in body
          while (and (listp form) (eq 'declare (first form)))
          collect form)
     
     (atomic
      ,@(if (symbolp func-name)
            ;; support (return-from ,func-name ...)
            `((block ,func-name
                ;; support (declare ...)
                (let ()
                  ,@body)))
            body))))



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

  (if body
      `(run-atomic (lambda () ,@body))
      `(values)))


(defmacro maybe-yield-before-rerun ()
  #-always nil
  #+never  (thread-yield))

(defun run-once (tx log)
  "Internal function invoked by RUN-ATOMIC and RUN-ORELSE2.

Run once the function TX inside a transaction,
using LOG as its transaction log."
  (declare (type function tx)
           (type tlog log))

  (with-recording-to-tlog log
    (prog ((parent (tlog-parent log)))

     run
     (set-tlog-version log)

     (log.debug "Tlog ~A {~A} starting~A" (~ log) (~ tx)
                (if parent
                    (format nil ", parent is tlog ~A" (~ parent))
                    ""))
     (restart-case
         (return (funcall tx))

       (rerun-once ()
         (log.trace "Tlog ~A {~A} will rerun" (~ log) (~ tx))
         (new-or-clear-tlog log :parent parent)
         (maybe-yield-before-rerun)
         (go run))))))


(defun run-atomic (tx)
  "Function equivalent of the ATOMIC macro.

Run the function TX inside a transaction.
If the transaction is invalid (conflicts) re-run TX immediately, ignoring
any error it may signal.

Otherwise, commit if TX returns normally, or rollback if it signals an error.

Finally, if TX called (retry), re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  (when (current-tlog)
    (return-from run-atomic (funcall tx)))

  (prog ((log (new-tlog)))

   run
   (handler-bind ((retry-error
                   (lambda (err)
                     (declare (ignore err))
                     (go retry)))
                  
                  (rerun-error
                   (lambda (err)
                     (declare (ignore err))
                     (invoke-restart 'rerun-once))))
     (return
       (multiple-value-prog1
           (run-once tx log)

         (log.trace "Tlog ~A {~A} wants to commit" (~ log) (~ tx))

         ;; commit also checks if log is valid
         (if (commit log)
             ;; all done, prepare to return.
             ;; we are not returning TLOGs to the pool in case tx signaled an error,
             ;; but that's not a problem since the TLOG pool is just a speed optimization
             (free-tlog log)

             (progn
               (log.debug "Tlog ~A {~A} could not commit, re-running it" (~ log) (~ tx))
               (go rerun))))))

   retry
   (log.debug "Tlog ~A {~A} will sleep, then retry" (~ log) (~ tx))
   (wait-tlog log)
   (go rerun-no-yield)

   rerun
   (log.debug "Tlog ~A {~A} will re-run" (~ log) (~ tx))
   (maybe-yield-before-rerun)
   ;; fallthrough

   rerun-no-yield
   (new-or-clear-tlog log :parent (tlog-parent log))
   (go run)))




