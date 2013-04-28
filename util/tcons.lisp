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


(in-package :stmx.util)

;;;; ** Transactional cell holding two values. It is the STM equivalent of CONS cells.

(transactional
 (defclass tcons ()
   ((first :initarg :first :accessor first-of)
    (rest  :initarg :rest  :accessor rest-of))
   (:documentation "Transactional cell holding two values. It is the STM equivalent of CONS cells.
To use TCONS cells, see the functions TCONS, TLIST, TFIRST and TREST.")))

(deftype tlist () '(or tcons null))

(declaim (ftype (function (t t) (values tcons &optional)) tcons)
         (inline tcons))

(defun tcons (first rest)
  "Create and return a new TCONS."
  (new 'tcons :first first :rest rest))


(declaim (ftype (function (&rest t) (values tlist &optional)) tlist))

(defun tlist (&rest list)
  "Create and return a new TLIST, whose cells are TCONS."
  (if list
    (let* ((head (tcons nil nil))
           (pos head))
      (loop for cell on list
         do
           (setf (tfirst pos) (first cell))
           (when (rest cell)
             (let1 new-cell (tcons nil nil)
               (setf (trest pos) new-cell
                     pos new-cell))))
      head)
    nil))
           
               



(declaim (ftype (function (tlist) t) tfirst trest)
         (inline
           tfirst trest))

(defun tfirst (tlist)
  "Return the first element in a TCONS or TLIST."
  (when tlist
      (_ tlist first)))
  

(defun trest (tlist)
  "Return the rest element in a TCONS or TLIST."
  (when tlist
    (_ tlist rest)))
  


(declaim (ftype (function (t tcons) t) (setf tfirst) (setf trest)))

(transaction
 (defun (setf tfirst) (value tcons)
   "Set VALUE as the first element in a TCONS or non-null TLIST."
   (setf (_ tcons first) value)))

(transaction
 (defun (setf trest) (value tcons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST."
   (setf (_ tcons rest) value)))



(defmacro tpush (value place)
  "Equivalent to PUSH, but for TCONS transactional cells.
Inserts VALUE as the first element in PLACE.
Return the modified PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (let1 value-to-push (gensym (symbol-name 'value-to-push-))
      `(let* ((,value-to-push ,value)
              ,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,(first stores) (tcons ,value-to-push ,get-form)))
         ,store-form))))


(defmacro tpop (place)
  "Equivalent to POP, but for TCONS transactional cells.
Removes and returns the first element in PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (let1 get-place (gensym (symbol-name 'get-place-))
      `(let* (,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,get-place ,get-form)
              (,(first stores) (trest ,get-place)))
         ,store-form
         (tfirst ,get-place)))))


(defprint-object (obj tcons :type nil :identity nil)
  (write-string "(")
  (when (tfirst obj)
    (loop for value = (tfirst obj)
       for rest = (trest obj)
       do
         (format t "~A" value)
         (unless (typep rest 'tcons)
           (unless (null rest)
             (format t " . ~A" rest))
           (return))
         (write-string " ")
         (setf obj rest)))
  (write-string ")"))
  
       