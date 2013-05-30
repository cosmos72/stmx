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

;;;; ** Transactional simple-vector: fixed-size one dimensional array

(deftype simple-tvector (&optional length) 
  "SIMPLE-TVECTOR is a transactional, one dimensional array.
It is currently a deftype, not a class or struct:
methods cannot be specialized on it."
  `(simple-vector ,(or length '*)))

(defun simple-tvector (length &key (element-type t)
                       (initial-element 0) initial-contents)
  "Create and return a new SIMPLE-TVECTOR."
  (declare (type fixnum length)
           (type list initial-contents)
           (ignore element-type))
  (let1 tvec (make-array length :element-type 'tvar :initial-element +dummy-tvar+)
    (if initial-contents
        (loop for i from 0 to (1- length)
           for cell = initial-contents then (rest cell)
           for element = (first cell) do
             (setf (svref tvec i) (tvar element)))
        (dotimes (i length)
          (setf (svref tvec i) (tvar initial-element))))
    tvec))

(declaim (inline simple-tvector-length))
(defun simple-tvector-length (tvec)
  "Return the length of simple-tvector TVEC."
  (declare (type simple-tvector tvec))
  (length tvec))


(declaim (inline tsvref (setf tsvref) tsvref-x (setf tsvref-tx) tsvref-notx (setf tsvref-notx)))

(defun tsvref (tvec index)
  "Return the INDEX-th element of simple-tvector TVEC.
Works both inside and outside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  ($ (svref tvec index)))

(defun (setf tsvref) (value tvec index)
  "Set the INDEX-th element of simple-tvector TVEC to VALUE.
Works both inside and outside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  (setf ($ (svref tvec index)) value))


(defun tsvref-tx (tvec index)
  "Return the INDEX-th element of simple-tvector TVEC.
Works ONLY inside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  ($-tx (svref tvec index)))

(defun (setf tsvref-tx) (value tvec index)
  "Set the INDEX-th element of simple-tvector TVEC to VALUE.
Works ONLY inside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  (setf ($-tx (svref tvec index)) value))


(defun tsvref-notx (tvec index)
  "Return the INDEX-th element of simple-tvector TVEC.
Works ONLY outside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  ($-notx (svref tvec index)))

(defun (setf tsvref-notx) (value tvec index)
  "Set the INDEX-th element of simple-tvector TVEC to VALUE.
Works ONLY outside transactions"
  (declare (type simple-tvector tvec)
           (type fixnum index))
  (setf ($-notx (svref tvec index)) value))


(defmacro do-simple-tvector ((element) tvec &body body)
  "Execute BODY on each ELEMENT contained in simple-tvector TVEC.

Creates an implicit block named NIL, so (return ...) can be used
to exit early from the loop with an explicit return value."
  (with-gensym var
    `(loop for ,var across ,tvec
        for ,element = ($ ,var) do
          (progn ,@body))))


;; simple-tvector is a deftype, not a class or struct:
;; cannot specialize methods on it
#|
 (defprint-object (tvec simple-tvector :identity nil)
  (dotimes (i (length tvec))
    (unless (zerop i)
      (write-string " "))
    (let1 var (svref tvec i)
      (multiple-value-bind (value present?) (peek-$ var)
        (if present?
            (format t "~A" value)
            (write-string "unbound"))))))
|#