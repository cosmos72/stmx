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

;;;; ** Defining

(defmacro transaction ((defun-or-defmethod func-name args &body body))
  "Deprecated. Use ATOMIC instead.

Define FUNC-NAME as a new atomic function or method.
Use this macro to wrap a normal DEFUN or DEFMETHOD as follows:
\(TRANSACTION (DEFUN function-name (arguments) body))
or
\(TRANSACTION (DEFMETHOD function-name (arguments) body))

The effect is the same as DEFUN - or DEFMETHOD - plus:
- the BODY is wrapped inside (atomic ...)"

  (let* ((docstring (when (and (stringp (first body)) (rest body))
                     (pop body)))
         declarations)

     (loop for form = (first body)
        while (and (consp form) (eq 'declare (first form)))
        do
          (push (pop body) declarations))
     (setf declarations (nreverse declarations))
     
     `(,defun-or-defmethod ,func-name ,args
        ;; move docstring here
        ,@(when docstring (list docstring))
        ;; also move all declarations
        ,@declarations
     
        (atomic
         ,(if (symbolp func-name)
              ;; support (return-from ,func-name ...)
              `(block ,func-name
                 ,@body)
              `(progn
                 ,@body))))))



;;;; ** Running


(defmacro sw-atomic (&rest body)
  "Run BODY in a software memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads."
  (if body
      `(run-sw-atomic (lambda () (block nil (locally ,@body))))
      `(values)))


(defmacro fast-sw-atomic (&rest body)
  (if body
      (let1 form `(block nil (locally ,@body))
        `(if (transaction?)
             ,form
             (%run-sw-atomic (lambda () ,form))))
      `(values)))

       

(declaim (inline run-once))

(defun run-once (tx log)
  "Internal function invoked by RUN-SW-ATOMIC and RUN-ORELSE.

Run once the function TX inside a transaction,
using LOG as its transaction log."
  (declare (type function tx)
           (type tlog log))

  (with-recording-to-tlog log
     (log.debug "Tlog ~A {~A} starting~A" (~ log) (~ tx) 
                (if-bind parent (tlog-parent log)
                    (format nil ", parent is tlog ~A" (~ parent))
                    ""))
     (funcall tx)))





(defun %run-sw-atomic (tx)
  "Function equivalent of the SW-ATOMIC macro.

Run the function TX inside a transaction.
If the transaction is invalid (conflicts) re-run TX immediately, ignoring
any error it may signal.

Otherwise, commit if TX returns normally, or rollback if it signals an error.

Finally, if TX called (retry), re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  (prog ((log (new-tlog))
         (aborted nil))

   (declare (type tlog log)
            (type boolean aborted))

   run
   (setf (tlog-read-version log)
         (if aborted
             (global-clock/sw/after-abort)
             (global-clock/sw/start-read)))

   (handler-case
       (return
         (multiple-value-prog1 (run-once tx log)

           (log.trace "Tlog ~A {~A} wants to commit" (~ log) (~ tx))

           ;; commit also checks if log is valid
           (if (commit log)
               ;; all done, prepare to return.
               ;; we are not returning TLOGs to the pool in case tx signaled an error,
               ;; but that's not a problem since the TLOG pool is just a speed optimization
               (free-tlog log)

               (progn
                 (log.debug "Tlog ~A {~A} could not commit, re-running it" (~ log) (~ tx))
                 (go rerun)))))

     (retry-error () (global-clock/sw/stat-committed) (go retry))
     (rerun-error () (global-clock/sw/stat-aborted)   (go rerun)))

   retry
   (log.debug "Tlog ~A {~A} will sleep, then retry" (~ log) (~ tx))
   (wait-tlog log)
   (setf aborted nil)
   (go rerun-no-yield)

   rerun
   (log.debug "Tlog ~A {~A} will re-run" (~ log) (~ tx))
   (maybe-yield-before-rerun)
   (setf aborted t)
   ;; fallthrough

   rerun-no-yield
   (log.trace "before rerun tlog ~A {~A}, before (new-or-clear-tlog)" (~ log) (~ tx))
   (new-or-clear-tlog log :parent (tlog-parent log))
   (log.trace "before rerun tlog ~A {~A}, after (new-or-clear-tlog)" (~ log) (~ tx))
   (go run)))


(declaim (inline run-sw-atomic))

(defun run-sw-atomic (tx)
  "Function equivalent of the SW-ATOMIC macro.

Run the function TX inside a transaction.
If the transaction is invalid (conflicts) re-run TX immediately, ignoring
any error it may signal.

Otherwise, commit if TX returns normally, or rollback if it signals an error.

Finally, if TX called (retry), re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  (if (transaction?)
      (funcall tx)
      (%run-sw-atomic tx)))

