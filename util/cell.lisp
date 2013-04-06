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

(defgeneric full?  (place)
  (:documentation "Return t if PLACE contains a value, otherwise return nil."))

(defgeneric empty? (place)
  (:documentation "Return nil if PLACE contains a value, otherwise return t."))

(defgeneric empty! (place)
  (:documentation "Remove any value contained in PLACE. Return PLACE."))

(defgeneric peek   (place &optional default)
  (:documentation "Return value stored in PLACE without removing it, and t as multiple values.
Return (values DEFAULT nil) if PLACE contains no value."))

(defgeneric take   (place)
  (:documentation "Wait until PLACE contains a value, then remove and return it."))

(defgeneric put    (place value)
  (:documentation "Wait until PLACE contains no value, then store VALUE in it and return VALUE."))

(defgeneric try-take (place)
  (:documentation "If PLACE contains a value, remove it and return t and it as multiple values.
Otherwise return (values nil nil)"))

(defgeneric try-put  (place value)
  (:documentation "If PLACE contains no value, store VALUE it and return t and VALUE
as multiple values. Otherwise return (values nil nil)"))



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

