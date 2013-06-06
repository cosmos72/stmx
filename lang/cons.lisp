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

;;;; ** CONS pool

(declaim (type list *cons-pool*))
(defvar *cons-pool* nil)

(eval-when (:load-toplevel :execute)
  (save-thread-initial-bindings *cons-pool*))

(declaim (ftype (function (&optional t t) cons) cons^)
         (ftype (function (cons)          null) free-cons^)
         (inline cons^
                 free-cons^))

(defun cons^ (&optional a b)
  "Get a CONS from free cons pool, otherwise allocate it. Return the CONS."
  (if-bind cell *cons-pool*
    (progn
      (setf *cons-pool* (rest cell)
            (first cell) a
            (rest  cell) b)
      cell)
    (cons a b)))

(defun free-cons^ (cell)
  "Add a CONS cell to free cons pool."
  (declare (type cons cell))
  (setf (first cell) nil
        (rest  cell) *cons-pool*
        *cons-pool*  cell)
  nil)



(defmacro push^ (value place)
  "Equivalent to PUSH, but uses CONS pool to speedup allocation.
Inserts VALUE as the first element in PLACE.
Return the modified PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (let1 value-to-push (gensym (symbol-name 'value-to-push-))
      `(let* ((,value-to-push ,value)
              ,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,(first stores) (cons^ ,value-to-push ,get-form)))
         ,store-form))))


(defmacro pop-free-cons^ (place)
  "Equivalent to POP, but also assumes the CONS at PLACE is no longer
used and can be added to free CONS pool.
Removes and returns the first element in PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (let1 get-place (gensym (symbol-name 'get-place-))
      `(let* (,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,get-place ,get-form)
              (,(first stores) (rest ,get-place)))
         (prog1
             (first ,get-place)
           ,store-form
           (free-cons ,get-place))))))
