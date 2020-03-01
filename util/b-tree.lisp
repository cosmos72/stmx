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

;;;; ** non-transactional b-tree

(declaim (inline %make-b-tree %b-tree-root (setf %b-tree-root)
		 %b-tree-pred-func))

(defstruct (b-tree (:constructor %make-b-tree) (:conc-name %b-tree-))
  (root      nil :type (or null tvar b-node))
  (pred-func (error "missing ~S argument instantiating ~S" 'pred 'b-tree)
	     :type function :read-only t)
  (pred-sym  nil :type symbol :read-only t))


(defun make-b-tree (pred)
  "Instantiate a B-TREE, using function named PRED to order its keys"
  (declare (type symbol pred))
  (%make-b-tree :pred-func (fdefinition pred) :pred-sym pred))




(defun get-b-tree (b key &optional default)
  "Find KEY in B-TREE B and return its value and T as multiple values.
If M does not contain KEY, return (values DEFAULT NIL)."
  (declare (type b-tree))
  (let ((node (%b-tree-root b))
        (pred (%b-tree-pred-func b)))
    (loop while node
       for xkey = (_ node key) do
         (case (compare-keys pred key xkey)
           (#.k< (setf node (_ node left)))
           (#.k> (setf node (_ node right)))
           (t    (return-from get-gmap (values (_ node value) t)))))
    (values default nil)))

