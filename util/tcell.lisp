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

;;;; ** Transactional cell, it can be empty or hold a single value

(defvar *empty-tcell* (gensym "EMPTY"))

(transactional
 (defclass tcell ()
   ((value :accessor value-of
           :initarg :value
           :initform *empty-tcell*))))

;; no need to wrap empty? in a transaction:
;; value-of is atomic, transaction aware, and performs a single read
(defmethod empty? ((cell tcell))
  (eq (value-of cell) *empty-tcell*))

(transaction
 (defmethod empty! ((cell tcell))
   "Remove value from CELL. Return CELL."
   (setf (value-of cell) *empty-tcell*)
   cell))

;; no need to specialize (full?) on CELLs: the method in cell.lisp is enough
;;
;; (defmethod full? ((cell cell))
;;   (not (empty? cell)))


;; no need to wrap peek in a transaction:
;; value-of is atomic, transaction aware, and performs a single read
(defmethod peek ((cell tcell) &optional default)
  (let1 value (value-of cell)
    (if (eq value *empty-tcell*)
        (values default nil)
        (values value t))))


(transaction
 (defmethod take ((cell tcell))
   (let1 value (value-of cell)
     (if (eq value *empty-tcell*)
         (retry)
         (progn
           (setf (value-of cell) *empty-tcell*)
           value)))))

(transaction
 (defmethod put ((cell tcell) value)
   (if (empty? cell)
       (setf (value-of cell) value)
       (retry))))

(transaction
 (defmethod try-take ((cell tcell))
   "hand-made, nonblocking version of (take place) for cells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-take place) which calls
\(atomic (nonblocking (take place)))"
   (let1 value (value-of cell)
     (if (eq value *empty-tcell*)
         nil
         (progn
           (setf (value-of cell) *empty-tcell*)
           (values t value))))))

(transaction
 (defmethod try-put ((cell tcell) value)
   "hand-made, nonblocking version of (put place) for tcells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-put place) which calls
\(atomic (nonblocking (put place value)))"
   (if (empty? cell)
       (values t (setf (value-of cell) value))
       nil)))


;;;; ** Printing

(defprint-object (obj tcell)
  ;; (value-of obj) works both inside and outside transactions.
  (let1 value (value-of obj)
    (if (eq value *empty-tcell*)
        (format t "empty")
        (format t "[~A]" value))))
