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

(defmacro transactional ((defclass class direct-superclasses direct-slots &rest class-options))
  "Define a new transactional class called CLASS.

use this macro to wrap a normal defclass as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))
the effect is the same as DEFCLASS, plus the default metaclass is
TRANSACTIONAL-CLASS, slots are transactional by default, and it inherits
from TRANSACTIONAL-OBJECT by default."
;  (let1 direct-superclasses (or direct-superclasses '(transactional-object))
    `(eval-always
       (,defclass ,class ,direct-superclasses
         ,direct-slots
         ,@class-options
         (:metaclass transactional-class))))


(defmacro transaction ((defun-or-defmethod func args &body body))
  "Define a new atomic function or method called FUNC.

use this macro to wrap a normal defun or defmethod as follows:
\(TRANSACTION (DEFUN function-name (arguments) body))
the effect is the same as DEFUN - or DEFMETHOD, plus the body is wrapped
inside (atomic ...)."
    `(eval-always
       (,defun-or-defmethod ,func ,args
         (atomic :id ',func
           ,@body))))



;;;; ** Retrying

(define-condition retry-error (error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'retry 'atomic))))


(define-condition rerun-error (error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'rerun 'orelse)))
  (:documentation "Signalled by ORELSE to force re-executing the parent transaction."))




(defun retry ()
  "Abort the current transaction and re-execute it from scratch.

Before re-executing, the transaction will wait on all variables
that have been read so far during the transaction
until at least one of them changes."
  (error 'retry-error))


(defun rerun ()
  "Abort the current transaction and immediately re-execute it from scratch
without waiting. Used by ORELSE."
  (error 'rerun-error))

  


;;;; ** Running


(defmacro atomic (&rest body)
  (let1 id (when (eq :id (first body))
             (pop body)
             (pop body))
    `(run-atomic (lambda () ,@body) :id ,id)))



(defun run-once (tx log)
  (declare (type function tx)
           (type tlog log))

  (with-recording-to-tlog log
    (let1 parent (parent-of log)

      (prog (x-retry? x-error x-values)

       execute
       (when (log:debug)
         (if parent
             (log:debug "Tlog ~A {~A} starting, parent is tlog ~A"
                        (~ log) (~ tx) (~ parent))
             (log:debug "Tlog ~A {~A} starting" (~ log) (~ tx))))

       (handler-case
           (progn
             (setf x-values (multiple-value-list (funcall tx)))
             (log:trace "Tlog ~A {~A} wants to commit, returned: ~{~A ~}"
                        (~ log) (~ tx) x-values))

         (rerun-error (err)
           (declare (ignore err))
           (log:trace "Tlog ~A {~A} wants to rerun" (~ log) (~ tx))
           (clear-tlog log :parent parent)
           (setf x-values nil) ;; paranoia
           (go execute))
         
         (retry-error (err)
           (declare (ignore err))
           (setf x-retry? t)
           (log:trace "Tlog ~A {~A} wants to retry" (~ log) (~ tx)))

         (t (err)
           (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                      (~ log) (~ tx) (type-of err) (~ err))
           (setf x-error err)))

       done ;; fallthrough
       (return-from run-once (values x-retry? x-error x-values))))))



(defun run-atomic (tx &key id)
  (declare (type function tx))

  (when (and (recording?) (current-tlog))
    (return-from run-atomic (funcall tx)))

  (when id
    (setf (~ tx) id))

  (let1 log (new 'tlog)

    (prog (x-values)

     execute
     (multiple-value-bind (retry? err values) (run-once tx log)
       (when (invalid? log)
         (log:debug "Tlog ~A {~A} is invalid, re-running it" (~ log) (~ tx))
         (go re-execute))
       (when retry?
         (log:debug "Tlog ~A {~A} will sleep, then retry" (~ log) (~ tx))
         (wait-tlog log)
         (go re-execute))
       (when err
         (log:debug "Tlog ~A {~A} will rollback and signal ~A" (~ log) (~ tx) (type-of err))
         (error err))

       (setf x-values values)
       (go commit))
   
     re-execute
     (clear-tlog log)
     (go execute)

     commit
     (if (commit log)
         (go done)
         (progn
           (log:debug "Tlog ~A {~A} could not commit, re-running it"
                      (~ log) (~ tx))
           (go re-execute)))
     
     done
     (return-from run-atomic (values-list x-values)))))




