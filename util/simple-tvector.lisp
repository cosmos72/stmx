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

(defstruct (simple-tvector (:constructor %make-simple-tvector))
  (vec #() :type simple-vector :read-only t))

(defun simple-tvector (length &key (element-type t)
		       (initial-element 0) initial-contents)
  "Create and return a new SIMPLE-TVECTOR."
  (declare (type fixnum length)
	   (type list initial-contents)
	   (ignore element-type))
  (let1 vec (make-array length :element-type 'tvar :initial-element +dummy-tvar+)
    (if initial-contents
	(loop for i from 0 to (1- length)
	   for cell = initial-contents then (rest cell)
	   for element = (first cell) do
	     (setf (svref vec i) (tvar element)))
	(dotimes (i length)
	  (setf (svref vec i) (tvar initial-element))))
    
    (%make-simple-tvector :vec vec)))

(defun simple-tvector-length (tvec)
  "Return the length of simple-tvector TVEC."
  (declare (type simple-tvector tvec))
  (length (simple-tvector-vec tvec)))


(declaim (inline tsvref (setf tsvref)))

(defun tsvref (tvec index)
  "Return the INDEX-th element of simple-tvector TVEC."
  (declare (type simple-tvector tvec)
	   (type fixnum index))
  ($ (svref (simple-tvector-vec tvec) index)))

(defun (setf tsvref) (value tvec index)
  "Set the INDEX-th element of simple-tvector TVEC to VALUE."
  (declare (type simple-tvector tvec)
	   (type fixnum index))
  (setf ($ (svref (simple-tvector-vec tvec) index)) value))


(defmacro do-simple-tvector ((element) tvec &body body)
  "Execute BODY on each ELEMENT contained in simple-tvector TVEC.

Creates an implicit block named NIL, so (return ...) can be used
to exit early from the loop with an explicit return value."
  (with-gensyms (vec var)
    `(let1 ,vec (simple-tvector-vec ,tvec)
       (loop for ,var across ,vec
	  for ,element = ($ ,var) do
	    (progn ,@body)))))


(defprint-object (tvec simple-tvector :identity nil)
  (let1 vec (simple-tvector-vec tvec)
    (dotimes (i (length vec))
      (unless (zerop i)
	(write-string " "))
      (let1 var (svref vec i)
	(multiple-value-bind (value present?) (peek-$ var)
	  (if present?
	      (format t "~A" value)
	      (write-string "unbound")))))))
