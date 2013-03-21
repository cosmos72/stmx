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

;;;; ** Abstract functions for concurrent cells

(defgeneric full?  (place))
(defgeneric empty? (place))
(defgeneric empty! (place))

(defgeneric take     (place))
(defgeneric try-take (place))
(defgeneric put      (place value))
(defgeneric try-put  (place value))



;; no need to wrap (defmethod full? ...) in (transaction ...)
;; or in an atomic block: it just calls (not (empty?));
;; (empty?) must be already atomic and (full?) does not read or write
;; other transactional memory
(defmethod full? (place)
  (not (empty? place)))

(transaction
 (defmethod try-take (place)
   "this method shows a general technique to convert a blocking, atomic operation
into a nonblocking, atomic one: simply wrap it in (atomic (nonblocking ...))"
   (nonblocking
     (take place))))

(transaction
 (defmethod try-put (place value)
   "this method shows a general technique to convert a blocking, atomic operation
into a nonblocking, atomic one: simply wrap it in (atomic (nonblocking ...))"
   (nonblocking
     (put place value))))

