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

;;;; ** Concurrent cell implemented with a transactional object

(defvar *empty-cell* (gensym "EMPTY"))

(transactional
 (defclass cell ()
   ((value :accessor value-of
           :initarg :value
           :initform *empty-cell*))))

;; no need to wrap empty? in a transaction:
;; value-of is atomic, transaction aware, and performs a single read
(defmethod empty? ((cell cell))
  (eq (value-of cell) *empty-cell*))

(transaction
 (defmethod empty! ((cell cell))
   "Remove value from CELL. Return CELL."
   (setf (value-of cell) *empty-cell*)
   cell))

;; no need to specialize (full?) on CELLs: the method in cell.lisp is enough
;;
;; (defmethod full? ((cell cell))
;;   (not (empty? cell)))


;; no need to wrap peek in a transaction:
;; value-of is atomic, transaction aware, and performs a single read
(defmethod peek ((cell cell) &optional default)
  (let1 value (value-of cell)
    (if (eq value *empty-cell*)
        (values default nil)
        (values value t))))


(transaction
 (defmethod take ((cell cell))
   (let1 value (value-of cell)
     (if (eq value *empty-cell*)
         (retry)
         (progn
           (setf (value-of cell) *empty-cell*)
           value)))))

(transaction
 (defmethod put ((cell cell) value)
   (if (empty? cell)
       (setf (value-of cell) value)
       (retry))))

(transaction
 (defmethod try-take ((cell cell))
   "hand-made, nonblocking version of (take place) for cells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-take place) which calls
\(atomic (nonblocking (take place)))"
   (let1 value (value-of cell)
     (if (eq value *empty-cell*)
         nil
         (progn
           (setf (value-of cell) *empty-cell*)
           (values t value))))))

(transaction
 (defmethod try-put ((cell cell) value)
   "hand-made, nonblocking version of (put place) for cells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-put place) which calls
\(atomic (nonblocking (put place value)))"
   (if (empty? cell)
       (values t (setf (value-of cell) value))
       nil)))


;;;; ** Printing

(defprint-object (obj cell)
  ;; do NOT use (empty? obj) here, it would start a transaction!
  ;; (value-of obj) is much better: it works both inside and outside transactions.
  (let1 value (value-of obj)
    (if (eq value *empty-cell*)
        (format t "empty")
        (format t "[~A]" value))))
