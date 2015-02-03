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


(in-package :stmx.lang)

(defun mapappend (fn seq)
  (apply #'append (mapcar fn seq)))

(defun collect-superclasses (class)
  (remove-duplicates
   (cons class
         (mapappend #'collect-superclasses
                    (closer-mop:class-direct-superclasses class)))))

(defun collect-all-superclasses (classes-names)
  (remove-duplicates
   (mapappend #'collect-superclasses (mapcar #'find-class classes-names))))

(defun local-precedence-ordering (class)
  (loop for cell on (cons class (closer-mop:class-direct-superclasses class))
     for class1 = (first cell)
     for class2 = (second cell)
     while class2
     collect (list class1 class2)))

(defun topological-sort (elements constraints tie-breaker error-msg)
  (let ((remaining-elements (copy-list elements))
        (remaining-constraints (copy-list constraints))
        (result nil))
    (loop
       (log:trace "~%elements = ~A~%constraints = ~A" elements constraints)
       (let ((minimal-elements
              (remove-if (lambda (class) (member class remaining-constraints :key #'second))
                         remaining-elements)))
         (when (null minimal-elements)
           (if (null remaining-elements)
               (return-from topological-sort (nreverse result))
               (error error-msg)))
         (let ((choice (if (null (rest minimal-elements))
                           (first minimal-elements)
                           (funcall tie-breaker
                                    minimal-elements
                                    result))))
           (log:trace "choice = ~A" choice)
           (push choice result)
           (setf remaining-elements (delete choice remaining-elements)
                 remaining-constraints (delete choice
                                               remaining-constraints
                                               :test #'member)))))))
        

(defun clos-tie-breaker-rule (minimal-elements reverse-precedence-list)
  (log:trace "~%minimal-elements = ~A~%reverse-pcl = ~A" minimal-elements reverse-precedence-list)
  (dolist (class reverse-precedence-list)
    (let* ((superclasses (closer-mop:class-direct-superclasses class))
           (common (intersection minimal-elements superclasses)))
      (when common
        (return-from clos-tie-breaker-rule (first common)))))
  (first minimal-elements))

(defun clos-compute-class-precedence-list (class-name direct-superclasses-names)
  (declare (type symbol class-name)
           (type list direct-superclasses-names))
  (let ((classes-to-order (collect-all-superclasses direct-superclasses-names)))
    (topological-sort classes-to-order
                      (remove-duplicates
                       (mapappend #'local-precedence-ordering
                                  classes-to-order)
                       :test 'equal)
                      #'clos-tie-breaker-rule
                      (format nil "Inconsistent precedence list for class ~A" class-name))))