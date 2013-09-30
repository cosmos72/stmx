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


;; CMUCL: fix some buggy bordeaux-threads type declarations
#+cmucl (declaim (ftype (function (t)   t) bt:join-thread))
#+cmucl (declaim (ftype (function (t t) t) bt:condition-wait))


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
  (ensure-thread-initial-bindings '(*current-thread* . (current-thread)))

  (defun start-multithreading ()
    ;; on CMUCL, (bt:start-multiprocessing) is blocking!
    #-cmucl (bt:start-multiprocessing))

  (start-multithreading)

  (defvar *bt/join-thread/tested* nil)

  ;; testing (feature? 'bt/join-thread) signals an error on CMUCL :(
  (unless *bt/join-thread/tested*
    (setf *bt/join-thread/tested* t)
    (add-feature 'bt/join-thread
                 (let ((x (gensym)))
                   (if (eq x (bt:join-thread (bt:make-thread (lambda () x))))
                       :sane
                       :broken)))))



#?+(eql bt/join-thread :broken)
(defstruct wrapped-thread
  (result nil)
  (thread (current-thread) :type bt:thread))



(defun start-thread (function &key name (initial-bindings bt:*default-special-bindings*))

  #?+(eql bt/join-thread :sane)
  (make-thread function :name name :initial-bindings initial-bindings)

  #?-(eql bt/join-thread :sane)
  (let1 th (make-wrapped-thread)
        (setf (wrapped-thread-thread th)
              (make-thread (lambda ()
                             (setf (wrapped-thread-result th)
                                   (funcall function)))
                           :name name
                           :initial-bindings initial-bindings))
        th))

(defun wait4-thread (th)

  #?+(eql bt/join-thread :sane)
  (join-thread th)

  #?-(eql bt/join-thread :sane)
  (progn
    (join-thread (wrapped-thread-thread th))
    (wrapped-thread-result th)))

