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

(eval-always
  (enable-pf-reader))


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


(defun retry ()
  "Abort the current transaction and re-execute it from scratch.

Before re-executing, the transaction will wait on all variables
that have been read so far during the transaction
until at least one of them changes."
  (error 'retry-error))

  
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

    (when (log:debug)
      (let1 parent (parent-of log)
        (if parent
            (log:debug "Tlog ~A {~A} starting, parent is tlog ~A"
                       (~ log) (~ tx) (~ parent))
            (log:debug "Tlog ~A {~A} starting" (~ log) (~ tx)))))

    (let ((x-retry? nil)
          (x-error nil)
          (x-values nil))
      (handler-case
          (progn
            (setf x-values (multiple-value-list (funcall tx)))
            (log:trace "Tlog ~A {~A} wants to commit, returned: ~{~A ~}"
                       (~ log) (~ tx) x-values))
        (retry-error (err)
          (declare (ignore err))
          (setf x-retry? t)
          (log:trace "Tlog ~A {~A} wants to retry"
                     (~ log) (~ tx)))
        (t (err)
          (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                     (~ log) (~ tx) (type-of err) (~ err))
          (setf x-error err)))
      (values x-retry? x-error x-values))))


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
       (unless (valid? log)
         (log:debug "Tlog ~A {~A} is invalid, retrying immediately" (~ log) (~ tx))
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
           (log:debug "Tlog ~A {~A} could not commit, retrying immediately"
                      (~ log) (~ tx))
           (go re-execute)))
     
     done
     (return-from run-atomic (values-list x-values)))))




;;;; ** Composing

(define-condition orelse-error (error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to use ~A or ~A outside an ~A block" 'try 'orelse 'atomic))))


(defun run-orelse (tx1 tx2 &key id1 id2)
  (declare (type function tx1 tx2))

  (let1 parent-log (current-tlog)
    (unless parent-log
      (error 'orelse-error))

    (when id1
      (setf (~ tx1) id1))
    (when id2
      (setf (~ tx2) id2))

    (prog (log1 log2 x-values)

     execute-tx1
     (setf log1 (make-or-clear-tlog log1 :parent parent-log))

     (multiple-value-bind (retry? err values) (run-once tx1 log1)
       (when retry?
         (log:debug "Tlog ~A {~A} wants to retry, trying {~A}"
                    (~ log1) (~ tx1) (~ tx2))
         (go execute-tx2))
       (unless (valid? log1)
         (log:debug "Tlog ~A {~A} is invalid, trying {~A}"
                    (~ log1) (~ tx1) (~ tx2))
         (go execute-tx2))
       (when err
         (log:debug "Tlog ~A {~A} will rollback and signal"
                    (~ log1) (~ tx1))
         (error err))

       (setf x-values values)
       (go commit-tx1))
   
     commit-tx1
     (merge-tlogs parent-log log1)
     (log:debug "Tlog ~A {~A} merged with parent tlog ~A"
                (~ log1) (~ tx1) (~ parent-log))
     (go done)
   
     execute-tx2
     (setf log2 (make-or-clear-tlog log2 :parent parent-log))

     (multiple-value-bind (retry? err values) (run-once tx2 log2)
       (when retry?
         (log:trace "Tlog ~A {~A} wants to retry, sleeping then retrying both {~A} and {~A}"
                    (~ log2) (~ tx2) (~ tx1) (~ tx2))
         (go wait-reexecute))
       (unless (valid? log2)
         (log:debug "Tlog ~A {~A} is invalid, trying {~A}"
                    (~ log2) (~ tx2) (~ tx1))
         (go execute-tx1))
       (when err
         (log:debug "Tlog ~A {~A} will rollback and signal"
                    (~ log2) (~ tx2))
         (error err))
       
       (setf x-values values)
       (go commit-tx2))

     commit-tx2
     (merge-tlogs parent-log log2)
     (log:debug "Tlog ~A {~A} merged with parent tlog ~A"
                (~ log2) (~ tx2) (~ parent-log))
     (go done)

     wait-reexecute
     (unless (or (null log2) (compatible-tlogs log1 log2))
       (log:debug "Tlog ~A {~A} and tlog ~A {~A} are incompatible, not going to sleep"
                  (~ log1) (~ tx1) (~ log2) (~ tx2))
       (if (valid? log1)
           (progn
             (log:debug "Tlog ~A {~A} is invalid, retrying it"
                        (~ log1) (~ tx1))
             (go execute-tx1))
           (progn
             (log:debug "Tlog ~A {~A} deduced to be invalid, retrying it"
                        (~ log2) (~ tx2))
             (go execute-tx2))))

     (log:debug "Sleeping for both tlog ~A {~A} and tlog ~A {~A}"
                (~ log1) (~ tx1) (~ log2) (~ tx2))
     (wait-tlog (merge-tlogs log1 log2))
     (go execute-tx1)

     done
     (return-from run-orelse (values-list x-values)))))


(defmacro orelse (form1 form2 &key id1 id2)
  "Execute first FORM1, then FORM2 in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  `(run-orelse (lambda () ,form1)
               (lambda () ,form2)
               :id1 ,id1
               :id2 ,id2))
  

(defmacro try (&body body)
  "Execute each form in BODY from left to right in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."

  (reduce (lambda (x y) (list 'orelse x y)) body :from-end t))



(defun run-nonblocking (tx &key id)
  "Function version of NONBLOCKING macro: execute the function tx in a nested transaction and:
a) if the nested transaction succeeds (i.e. commits), return multiple values:
   T followed by the values returned by the transaction.
b) if the nested transaction signals an error, raise such error.
c) if the nested transaction attempts to retry,
   immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (declare (type function tx))
  (orelse (multiple-value-call #'values t (funcall tx))
          nil
          :id1 (if id id 'nonblocking)
          :id2 'nonblocking-return-nil))


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
    `(run-nonblocking (lambda () ,@body) :id ,id)))
