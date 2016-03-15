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

;;;; ** Printing utilities

(defgeneric print-object-contents (stream obj))

(defmethod print-object-contents ((stream (eql 'nil)) obj)
  (with-output-to-string (s)
    (print-object-contents s obj)))

(defmethod print-object-contents (stream obj)
  (format stream "~A" obj))

(defmethod print-object-contents (stream (obj hash-table))
  (format stream "{")
  (let1 first t
    (do-hash (key value) obj
      (format stream "~A~S=~S" (if first "" ", ") key value)
      (setf first nil)))
  (format stream "}"))
