;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
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

(enable-#?-syntax)

(deftype ufixnum () `(integer 0 ,most-positive-fixnum))

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



;;;; ** generic comparison

#+(and)
(eval-always
  (defconstant k< -1)
  (defconstant k=  0)
  (defconstant k> +1))

#-(and)
(eval-always
  (defconstant k< :<)
  (defconstant k= :=)
  (defconstant k> :>))

(deftype comp-result () `(member ,k< ,k= ,k>))

(declaim (inline compare-keys))
(defun compare-keys (pred key1 key2)
  "Compare KEY1 agains KEY2 using the comparison function PRED.
Return K< if KEY1 compares as lesser than KEY2,
return K> if KEY1 compares as greater than KEY2,
return K= if KEY1 and KEY2 compare as equal."
  (declare (type function pred))
  (the comp-result
    (cond
      ((funcall pred key1 key2) k<)
      ((funcall pred key2 key1) k>)
      (t k=))))




#?+sxhash-equalp
(defmacro %sxhash-equalp (x)
  (let ((form (get-feature 'sxhash-equalp)))
    (etypecase form
      (symbol (list form x))
      (cons   (substitute x '* form)))))

#?-sxhash-equalp
(defmacro %sxhash-equalp (x)
  #.(log:warn "missing SXHASH-EQUALP on this implementation,
  falling back on SXHASH.
  GHASH-TABLE and THASH-TABLE instances using :test 'EQUALP may not work properly.")
  `(sxhash ,x))


(declaim (inline sxhash-equalp))
(defun sxhash-equalp (x)
  "Variant of SXHASH designed for EQUALP tests, i.e.
\(equalp x y) implies (= (sxhash-equalp x) (sxhash-equalp y)).
A common use is for ghash-tables and thash-tables that use :test 'equalp"
  (%sxhash-equalp x))

;;;; ** Utility macros

;; for some reason, under certain circumstances SBCL invokes
;; slot-value-using-class only from slot accessors, not from (slot-value ...)

;; LispWorks is much more picky: slot accessors systematically bypass slot-value-using-class
;; UNLESS the DECLARED class for the object has the flag :optimize-slot-access nil
;; Instead, (slot-value ...) works fine in LispWorks.

(let ((pkg (find-package (symbol-name 'stmx.util))))
  (defmacro _ (obj slot)
    `(slot-value ,obj ',(if (eq pkg (symbol-package slot))
                            slot
                            (intern (symbol-name slot) pkg)))))

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




