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

