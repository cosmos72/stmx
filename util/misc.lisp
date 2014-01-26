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


(in-package :stmx.util)

;;;; ** Some simple functions optimized for FIXNUMs


(declaim (inline fixnum< fixnum> fixnum= fixnum/=))

(defun fixnum< (x y)
  "Optimized version of (< x y) for FIXNUM arguments"
  (declare (type fixnum x y))
  (the boolean (< x y)))

(defun fixnum> (x y)
  "Optimized version of (> x y) for FIXNUM arguments"
  (declare (type fixnum x y))
  (the boolean (> x y)))


(defun fixnum= (x y)
  "Optimized version of (= x y) for FIXNUM arguments"
  (declare (type fixnum x y))
  (the boolean (= x y)))

(defun fixnum/= (x y)
  "Optimized version of (/= x y) for FIXNUM arguments"
  (declare (type fixnum x y))
  (the boolean (/= x y)))




;;;; ** Utility macros

;; for some reason, under certain circumstances SBCL invokes
;; slot-value-using-class only from slot accessors, not from (slot-value ...)

;; LispWorks is much more picky: slot accessors systematically bypass slot-value-using-class
;; UNLESS the DECLARED class for the object has the flag :optimize-slot-access nil
;; Instead, (slot-value ...) works fine in LispWorks.

(defmacro _ (obj slot-name)
  `(slot-value ,obj ',slot-name))

#|
(eval-always
  (let1 of (symbol-name '-of)
    (defmacro _ (obj slot-name)
      (let1 accessor (intern (concatenate 'string (symbol-name slot-name) of))
        `(,accessor ,obj)))))
|#



(defmacro with-ro-slots ((&rest slots) instance &body body)
  (with-gensym obj
    `(let ((,obj ,instance))
       (let ,(loop for slot in slots
                collect `(,slot (_ ,obj ,slot)))
         ,@body))))


(defmacro with-rw-slots ((&rest slots) instance &body body)
  (with-gensym obj
    `(let ((,obj ,instance))
       (symbol-macrolet ,(loop for slot in slots
                            collect `(,slot (_ ,obj ,slot)))
         ,@body))))




