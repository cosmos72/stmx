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

(enable-#?-syntax)

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



  


;;;; * Wrappers around Bordeaux Threads to capture
;;;; * the return value of functions executed in threads

(declaim (type bt:thread *current-thread*))
(defvar *current-thread* (current-thread))

(eval-always
  (ensure-thread-initial-bindings '(*current-thread* . (current-thread))))


(eval-when (:compile-toplevel)
  
  (defvar *tested-join-thread* nil)
  (unless *tested-join-thread*
    (setf *tested-join-thread* t)

    (let ((x (gensym)))
      (when (eq x (bt:join-thread (bt:make-thread (lambda () x))))
        (add-feature 'bt.join-thread-is-sane)))))



#?-bt.join-thread-is-sane
(defstruct wrapped-thread
  (result nil)
  (thread (current-thread) :type thread))


(defun start-thread (function &key name (initial-bindings bt:*default-special-bindings*))

  #?+bt.join-thread-is-sane
  (make-thread function :name name :initial-bindings initial-bindings)

  #?-bt.join-thread-is-sane
  (let1 th (make-wrapped-thread)
    (setf (wrapped-thread-thread th)
          (make-thread (lambda ()
                         (setf (wrapped-thread-result th)
                               (funcall function)))
                       :name name
                       :initial-bindings initial-bindings))
    th))

#?+bt.join-thread-is-sane
(defun wait4-thread (th)
  (declare (type thread th))
  (join-thread th))


#?-bt.join-thread-is-sane
(defun wait4-thread (th)
  (declare (type wrapped-thread th))

  (join-thread (wrapped-thread-thread th))
  (wrapped-thread-result th))

