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


(in-package :stmx.test)


(defun hash-table-to-sorted-keys (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (sort (hash-table-keys hash) pred))


(defun hash-table-to-sorted-pairs (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (sort (hash-table-pairs hash) pred :key #'first))


(defun hash-table-to-sorted-values (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (loop for pair in (hash-table-to-sorted-pairs hash pred)
     collect (rest pair)))
         


