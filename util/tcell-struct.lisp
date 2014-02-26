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

(declaim (inline make-tcell))

(transactional-struct
 (defstruct tcell
   (value +unbound-tvar+)))




(declaim (ftype (function (&optional t) (values tcell &optional)) tcell))

(defun tcell (&optional (value +unbound-tvar+))
  "Create and return a new TCELL."
  (make-tcell :value value))


;; no need to wrap empty? in a transaction:
;; (tcell-value c) is atomic and transaction aware
(defmethod empty? ((c tcell))
  (eq (tcell-value c) +unbound-tvar+))


;; no need to wrap empty! in a transaction:
;; (setf (tcell-value c) value) is atomic and transaction aware
(defmethod empty! ((c tcell))
  "Remove value from CELL. Return CELL."
  (setf (tcell-value c) +unbound-tvar+)
  c)

;; no need to specialize (full?) on CELLs: the method in cell.lisp is enough
;;
;; (defmethod full? ((cell cell))
;;   (not (empty? cell)))


;; no need to wrap peek in a transaction:
;; (tcell-value c) is atomic and transaction aware
(defmethod peek ((c tcell) &optional default)
  (let1 value (tcell-value c)
    (if (eq value +unbound-tvar+)
        (values default nil)
        (values value t))))


(defmethod take ((c tcell))
  (fast-atomic
   (let1 value (tcell-value c)
     (if (eq value +unbound-tvar+)
         (retry)
         (progn
           (setf (tcell-value c) +unbound-tvar+)
           value)))))


(defmethod put ((c tcell) value)
  (fast-atomic
   (if (empty? c)
       (setf (tcell-value c) value)
       (retry))))


(defmethod try-take ((c tcell))
  "hand-made, nonblocking version of (take place) for cells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-take place) which calls
\(atomic (nonblocking (take place)))"
  (fast-atomic
   (let1 value (tcell-value c)
     (if (eq value +unbound-tvar+)
         nil
         (progn
           (setf (tcell-value c) +unbound-tvar+)
           (values t value))))))


(defmethod try-put ((c tcell) value)
  "hand-made, nonblocking version of (put place) for tcells.
less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-put place) which calls
\(atomic (nonblocking (put place value)))"
  (fast-atomic
   (if (empty? c)
       (values t (setf (tcell-value c) value))
       nil)))


;;;; ** Printing

(defprint-object (c tcell)
  ;; (value-of obj) works both inside and outside transactions.
  (let1 value (tcell-value c)
    (if (eq value +unbound-tvar+)
        (format t "empty")
        (format t "[~A]" value))))
