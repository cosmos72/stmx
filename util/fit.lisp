;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
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

(defun least-square-fit-3 (x1 y1 x2 y2 x3 y3)
  (let* ((n 3)
         (x (/ (+ x1 x2 x3) n))
         (y (/ (+ y1 y2 y3) n))
         (xx (/ (+ (* x1 x1) (* x2 x2) (* x3 x3)) n))
         (xy (/ (+ (* x1 y1) (* x2 y2) (* x3 y3)) n))
         (m (/ (- xy (* x y)) (- xx (* x x))))
         (q (- y (* m x))))
    (values m q)))



         
         