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

;;;; ** Transactional cell, it can be empty or hold a single value

(declaim (type symbol +unbound-tvar+))
(defconstant +empty-tcell+ '+empty-tcell+
  "Empty TCELL objects actually contain this symbol in their VALUE slot. Use with care.")

(transactional
 (defclass tcell ()
   ((value :initarg :value
           :initform +empty-tcell+))))

(declaim (ftype (function (&optional t) (values tcell &optional)) tcell))

(defun tcell (&optional (value +empty-tcell+))
  "Create and return a new TCELL."
  (new 'tcell :value value))

;; no need to wrap empty? in a transaction:
;; (_ cell value) is atomic, transaction aware, and performs a single read
(defmethod empty? ((cell tcell))
  (eq (_ cell value) +empty-tcell+))


(defmethod empty! ((cell tcell))
  "Remove value from CELL. Return CELL."
  (fast-atomic
   (setf (_ cell value) +empty-tcell+)
   cell))

;; no need to specialize (full?) on CELLs: the method in cell.lisp is enough
;;
;; (defmethod full? ((cell cell))
;;   (not (empty? cell)))


;; no need to wrap peek in a transaction:
;; (_ cell value) is atomic, transaction aware, and performs a single read
(defmethod peek ((cell tcell) &optional default)
  (let1 value (_ cell value)
    (if (eq value +empty-tcell+)
        (values default nil)
        (values value t))))


(defmethod take ((cell tcell))
  (fast-atomic
   (let1 value (_ cell value)
     (if (eq value +empty-tcell+)
         (retry)
         (progn
           (setf (_ cell value) +empty-tcell+)
           value)))))


(defmethod put ((cell tcell) value)
  (fast-atomic
   (if (empty? cell)
       (setf (_ cell value) value)
       (retry))))


(defmethod try-take ((cell tcell))
  "hand-made, nonblocking version of (take place) for cells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-take place) which calls
\(atomic (nonblocking (take place)))"
  (fast-atomic
   (let1 value (_ cell value)
     (if (eq value +empty-tcell+)
         nil
         (progn
           (setf (_ cell value) +empty-tcell+)
           (values t value))))))


(defmethod try-put ((cell tcell) value)
  "hand-made, nonblocking version of (put place) for tcells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-put place) which calls
\(atomic (nonblocking (put place value)))"
  (fast-atomic
   (if (empty? cell)
       (values t (setf (_ cell value) value))
       nil)))


;;;; ** Printing

(defprint-object (obj tcell)
  ;; (value-of obj) works both inside and outside transactions.
  (let1 value (_ obj value)
    (if (eq value +empty-tcell+)
        (format t "empty")
        (format t "[~S]" value))))

#-(and)
(defmethod print-object ((obj tcell) stream)
  (let1 value (_ obj value)
    (if (eq value +empty-tcell+)
        (format stream "#@(~S)" 'tcell)
        (format stream "#@(~S ~S ~S)" 'tcell :value value))))
