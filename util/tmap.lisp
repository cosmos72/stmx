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
   ((left  :initform nil) ;; :type (or null tnode))
    (right :initform nil) ;;  :type (or null tnode))
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


(defmethod get-bmap ((m tmap) key &optional default)
  (declare (ignore m key default))
  (run-atomic #'call-next-method))

(defmethod set-bmap ((m tmap) key value)
  (declare (ignore m key value))
  (run-atomic #'call-next-method))

(defmethod rem-bmap ((m tmap) key)
  (declare (ignore m key))
  (run-atomic #'call-next-method))

(defmethod clear-bmap ((m tmap))
  (declare (ignore m))
  (run-atomic #'call-next-method))

(defmethod add-to-bmap ((m tmap) &rest keys-and-values)
  (declare (ignore m keys-and-values))
  (run-atomic #'call-next-method))

(defmethod remove-from-bmap ((m tmap) &rest keys)
  (declare (ignore m keys))
  (run-atomic #'call-next-method))

(defmethod min-bmap ((m tmap))
  (declare (ignore m))
  (run-atomic #'call-next-method))

(defmethod max-bmap ((m tmap))
  (declare (ignore m))
  (run-atomic #'call-next-method))

(defmethod copy-bmap-into ((mcopy tmap) m)
  (declare (ignore mcopy m))
  (run-atomic #'call-next-method))

(defmethod copy-bmap-into (mcopy (m tmap))
  (declare (ignore mcopy m))
  (run-atomic #'call-next-method))

(defmethod map-bmap ((m tmap) func)
  (declare (ignore m func))
  (run-atomic #'call-next-method))

(defmethod map-bmap-from-end ((m tmap) func)
  (declare (ignore m func))
  (run-atomic #'call-next-method))

(defmethod bmap-keys ((m tmap) &optional to-list)
  (declare (ignore m to-list))
  (run-atomic #'call-next-method))

(defmethod bmap-values ((m tmap) &optional to-list)
  (declare (ignore m to-list))
  (run-atomic #'call-next-method))

(defmethod bmap-pairs ((m tmap) &optional to-alist)
  (declare (ignore m to-alist))
  (run-atomic #'call-next-method))



;;;; ** Abstract methods to be implemented by BMAP subclasses

(defmethod bmap/new-node ((m tmap) key value)
   (new 'tnode :key key :value value))


