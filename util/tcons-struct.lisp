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

;;;; ** alternative implementation of TCONS and TLIST. Uses structs instead of CLOS objects.

(declaim (inline make-tcons %stmx-impl/make-tcons))

(transactional-struct
 (defstruct tcons
   "Transactional cell holding two values. It is the STM equivalent of CONS cells.
To use TCONS cells, see the functions TCONS, TLIST, TFIRST and TREST."
   first
   rest))

(declaim (ftype (function (t t) (values tcons &optional)) tcons))

(defun tcons (first rest)
  "Create and return a new TCONS."
  (make-tcons :first first :rest rest))


(deftype tlist () '(or tcons null))

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
           
               



(declaim (ftype (function (#-ecl tlist #+ecl t) t) tfirst trest)
         (inline
           tfirst trest))

(defun tfirst (tlist)
  "Return the first element in a TCONS or TLIST."
  (when tlist
    (tcons-first tlist)))
  

(defun trest (tlist)
  "Return the rest element in a TCONS or TLIST."
  (when tlist
    (tcons-rest tlist)))
  


(declaim (ftype (function (t tcons) t) (setf tfirst) (setf trest))
         (inline (setf tfirst) (setf trest)))


(defun (setf tfirst) (value tcons)
  "Set VALUE as the first element in a TCONS or non-null TLIST.
This function should always be executed inside an STMX atomic block."
  (setf (tcons-first tcons) value))

(defun (setf trest) (value tcons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST.
This function should always be executed inside an STMX atomic block."
   (setf (tcons-rest tcons) value))



(defmacro tpush (value place)
  "Equivalent to PUSH, but for TCONS transactional cells.
Inserts VALUE as the first element in PLACE.
Return the modified PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (with-gensym value-to-push
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

