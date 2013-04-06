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

;;;; ** Transactional sorted map, implemented with red-black trees.
;;;; For a non-transactional version, see rbmap.lisp

(transactional
 (defclass tnode (rbnode)
   ;; override all inherited slots to make them transactional
   ((left  :initform nil   :type (or null tnode))
    (right :initform nil   :type (or null tnode))
    (key   :initarg :key)
    (value :initarg :value)
    (color :initform +red+ :type bit))

   (:documentation "Node of transactional sorted map, implemented with red-black tree")))


(transactional
 (defclass tmap (rbmap)
   ;; override inherited slots to make them transactional
   ((root  :initform nil :type (or null tnode))
    ;; inherited slot pred is immutable -> no need to make it transactional
    ;; -> no need to override it
    (count :initform 0   :type fixnum))
   (:documentation "Transactional sorted map, implemented with red-black tree")))



;;;; ** Public API. Most methods simply wrap (call-next-method) in a transaction


(transaction
 (defmethod get-bmap ((m tmap) key &optional default)
   (call-next-method)))

(transaction
 (defmethod set-bmap ((m tmap) key value)
   (call-next-method)))

(transaction
 (defmethod rem-bmap ((m tmap) key)
   (call-next-method)))

(transaction
 (defmethod clear-bmap ((m tmap))
   (call-next-method)))

(transaction
 (defmethod add-to-bmap ((m tmap) &rest keys-and-values)
   (call-next-method)))

(transaction
 (defmethod remove-from-bmap ((m tmap) &rest keys)
   (call-next-method)))

(transaction
 (defmethod min-bmap ((m tmap))
   (call-next-method)))

(transaction
 (defmethod max-bmap ((m tmap))
   (call-next-method)))

(transaction
 (defmethod copy-bmap-into ((mcopy tmap) m)
   (call-next-method)))

(transaction
 (defmethod copy-bmap-into (mcopy (m tmap))
   (call-next-method)))

(transaction
 (defmethod map-bmap ((m tmap) func)
   (call-next-method)))

(transaction
 (defmethod map-bmap-from-end ((m tmap) func)
   (call-next-method)))

(transaction
 (defmethod bmap-keys ((m tmap) &optional to-list)
   (call-next-method)))

(transaction
 (defmethod bmap-values ((m tmap) &optional to-list)
   (call-next-method)))

(transaction
 (defmethod bmap-pairs ((m tmap) &optional to-alist)
   (call-next-method)))



;;;; ** Abstract methods to be implemented by BMAP subclasses

(defmethod bmap/new-node ((m tmap) key value)
   (new 'tnode :key key :value value))


