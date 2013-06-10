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
   ;; override all inherited slots to make them transactional.
   ;; No need to specify :initform, :initarg, :accessor or :type
   ;; unless we want to override the settings found in superclasses

   ((left  ) ;; :type (or null tnode) ;; this sends SBCL in infinite recursion at (optimize (speed 3))
    (right ) ;; :type (or null tnode) ;; idem
    (key   )
    (value )
    (color ))
   (:documentation "Node of transactional sorted map, implemented with red-black tree")))


(transactional
 (defclass tmap (rbmap)
   ;; override inherited slots to make them transactional
   ((root :type (or null tnode))
    ;; inherited slot pred is immutable -> no need to make it transactional
    ;; -> no need to override it
    (count))
   (:documentation "Transactional sorted map, implemented with red-black tree")))



;;;; ** Public API: all functions/methods are inherited from rbmap

;;;; ** Abstract methods to be implemented by BMAP subclasses

(defmethod bmap/new-node ((m tmap) key value)
  (new 'tnode :key key :value value))


