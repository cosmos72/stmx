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


(in-package :stmx.test)

(def-suite accessors-suite :in suite)
(in-suite accessors-suite)

(transactional
 (defclass point ()
   ((x :initform 0 :initarg :x :accessor point-x)
    (y :initform 0 :initarg :y :accessor point-y))))

(defun point-slots (p)
  (atomic
   (values (slot-value p 'x)
           (slot-value p 'y))))


(defun point-readers (p)
  (atomic
   (values (point-x p)
           (point-y p))))

(defun point-readers-test ()
  (let ((p (make-instance 'point :x 1 :y 2)))
    (atomic
     (setf (slot-value p 'x) 3
           (slot-value p 'y) 4)
     (multiple-value-bind (xs ys) (point-slots p)
       (multiple-value-bind (xa ya) (point-readers p)
         (is (eql 3 xs))
         (is (eql 3 xa))
         (is (eql 4 ys))
         (is (eql 4 ya)))))))
     

(defun point-writers-test ()
  (let ((p (make-instance 'point :x 1 :y 2)))
    (atomic
     (setf (point-x p) 5
           (point-y p) 6)
     (multiple-value-bind (xs ys) (point-slots p)
       (multiple-value-bind (xa ya) (point-readers p)
         (is (eql 5 xs))
         (is (eql 5 xa))
         (is (eql 6 ys))
         (is (eql 6 ya)))))))
     

(def-test point-accessors (:compile-at :definition-time)
  (point-readers-test)
  (point-writers-test))
  
