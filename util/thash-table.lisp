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

;;;; * support class for THASH-TABLE

(transactional
 (defclass thash-pair (ghash-pair)
   ;; Override inherited slots to make them transactional.
   ;; Do NOT override the slots that must remain non-transactional.
   ;; No need to specify :initform, :initarg, :accessor or :type
   ;; unless we want to override the settings found in superclasses
   ((key)
    (value)
    (next))))



;;;; ** Transactional hash table

(transactional
 (defclass thash-table (ghash-table)
   ;; Override inherited slots to make them transactional.
   ;; Do NOT override the slots that must remain non-transactional.
   ;; No need to specify :initform, :initarg, :accessor or :type
   ;; unless we want to override the settings found in superclasses
   ((vec)
    (count))

   (:documentation "Transactional hash table.")))


(defmethod initialize-instance :after ((hash thash-table) &rest other-keys)
  (declare (ignore other-keys))

  (setf (_ hash aref-fun)     #'tsvref
        (_ hash set-aref-fun) #'%setf-tsvref)) ;; (setf tsvref) has a defsetf expansion :(








(defmethod ghash/new-pair ((hash thash-table) key value next)
  ;; Allocate a GHASH-PAIR, initialize it with KEY, VALUE and NEXT and return it.
  (declare (ignore hash)
           (type (or null thash-pair) next))

  (new 'thash-pair :key key :value value :next next))


(defmethod ghash/new-vec ((hash thash-table) capacity)
  ;; Allocate a new GHASH-VECTOR with length = CAPACITY,
  ;; initialize all its elements to NIL and return it.
  (declare (ignore hash)
           (type fixnum capacity))

  (simple-tvector capacity :initial-element nil))



