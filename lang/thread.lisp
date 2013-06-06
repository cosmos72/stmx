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


(in-package :stmx.lang)


;;;; ** Helpers to initialize thread-local variables

(eval-always
 (defun ensure-thread-initial-binding (sym form)
   (declare (type symbol sym)
            (type (or atom cons) form))
   (unless (assoc sym bt:*default-special-bindings* :test 'eq)
     (push (cons sym form) bt:*default-special-bindings*)))

 (defun ensure-thread-initial-bindings (&rest syms-and-forms)
   (declare (type list syms-and-forms))
   (loop for sym-and-form in syms-and-forms do
        (unless (assoc (first sym-and-form) bt:*default-special-bindings* :test 'eq)
          (push sym-and-form bt:*default-special-bindings*))))

 (defmacro save-thread-initial-bindings (&rest syms)
   `(ensure-thread-initial-bindings
     ,@(loop for sym in syms collect `(cons ',sym ,sym)))))



;;;; ** Faster replacement for bordeaux-threads:with-lock-held

(defmacro with-lock ((lock) &body body)
  "Faster replacement for BORDEAUX-THREADS:WITH-LOCK-HELD."
  (with-gensym lock-var
    `(let1 ,lock-var ,lock
       (bt:acquire-lock ,lock-var)
       (unwind-protect
            (progn ,@body)
         (bt:release-lock ,lock-var)))))
  


;;;; * Wrappers around Bordeaux Threads to capture the values returned by functions executed in threads

(declaim (type bt:thread *current-thread*))
(defvar *current-thread* (current-thread))

(eval-always
 (ensure-thread-initial-bindings '(*current-thread* . (current-thread))))


#-stmx.sane-bt.join-thread
(defstruct wrapped-thread
  (result nil)
  (thread (current-thread) :type thread))


(defun start-thread (function &key name (initial-bindings bt:*default-special-bindings*))

  #+stmx.sane-bt.join-thread
  (make-thread function :name name :initial-bindings initial-bindings)

  #-stmx.sane-bt.join-thread
  (let1 th (make-wrapped-thread)
    (setf (wrapped-thread-thread th)
          (make-thread (lambda ()
                         (setf (wrapped-thread-result th)
                               (funcall function)))
                       :name name
                       :initial-bindings initial-bindings))
    th))

#-stmx.sane-bt.join-thread
(defun wait4-thread (th)
  (declare (type wrapped-thread th))

  (join-thread (wrapped-thread-thread th))
  (wrapped-thread-result th))


#+stmx.sane-bt.join-thread
(defun wait4-thread (th)
  (declare (type thread th))
  (join-thread th))


