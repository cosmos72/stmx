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
   ((root  :initform nil :type (or null tnode))
    ;; inherited slot pred is immutable -> not transactional, no need to override
    (count :initform 0   :type fixnum))
   (:documentation "Transactional sorted map, implemented with red-black tree")))


#|
(defmethod bmap/new-node ((m tmap) key value)
   (new 'tnode :key key :value value))
|#


