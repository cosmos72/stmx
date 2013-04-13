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

;;;; ** Priority queue implemented with a binary min-heap.

(defvar *empty-vector* (make-array '(0)))

(defclass bheap ()
  ((vector :initarg :vector :initform *empty-vector*
	   :type vector :accessor vector-of)
   
   (length :initform 0
	   :type fixnum :accessor length-of)
   
   (key    :initarg :key  :initform #'identity 
	   :type function :reader key-of)

   (pred   :initarg :pred :initform (error "missing :pred argument instantiating ~A or a subclass" 'bheap) 
	   :type function :reader pred-of))

   (:documentation "Priority queue implemented with a binary min-heap.
Elements that compare smaller than others will be the first (top) in the heap."))



(defmethod initialize-instance :after ((q bheap) &key &allow-other-keys)
  "Initialize bheap Q."
  (setf (length-of q) (length (vector-of q)))
  (heapify-bheap q))
	     

;;;; ** bheap private functions

(defun compare-bheap-entries (q n1 n2)
  "Compare entries at positions N1 and N2 in bqueue Q."
  (declare (type bheap q)
	   (type fixnum n1 n2))
  (with-ro-slots (vector key pred) q
    (let ((key1 (funcall key (aref vector n1)))
	  (key2 (funcall key (aref vector n2))))
      ;; swap key1 and key2: if pred is #'<
      ;; we want smaller elements to come first
      (funcall pred key2 key1))))


(defun sift-down-bheap (q start end)
  (declare (type bheap q)
	   (type fixnum start end))

  (let ((root start)
	(vector (vector-of q)))
    (declare (type fixnum root))
    (loop for lchild = (the fixnum (1+ (* 2 root)))
       for swap = root
       while (<= lchild end) do
	 (when (compare-bheap-entries q swap lchild)
	   (setf swap lchild))
	 (let1 rchild (the fixnum (1+ lchild))
	   (when (and (<= rchild end)
		    (compare-bheap-entries q swap rchild))
	     (setf swap rchild)))
	 (when (= swap root)
	   (return))
	 (log:debug "vector = ~A, swapping index ~A with ~A" vector root swap)
	 (rotatef (aref vector root) (aref vector swap))
	 (setf root swap))
    (log:debug "vector = ~A, done with start index = ~A" vector start)))


(defun sift-up-bheap (q start end)
  (declare (type bheap q)
	   (type fixnum start end))

  (let ((vector (vector-of q))
	(child end))
    (declare (type fixnum child))
    (loop while (< start child)
       for parent = (the fixnum (floor (1- child) 2)) do
	 (unless (compare-bheap-entries q parent child)
	   (return))
	 (log:debug "vector = ~A, swapping index ~A with ~A" vector parent child)
	 (rotatef (aref vector parent) (aref vector child))
	 (setf child parent))
    (log:debug "vector = ~A, done with start index = ~A" vector start)))



(defun heapify-bheap (q)
  "Establish heap invariant in bheap Q. Return Q.
Destructively modifies (vector-of Q)."
  (declare (type bheap q))

  (with-ro-slots (length) q
    (loop for start = (the fixnum (1- (floor length 2))) ;; index of last parent
	 #||#        then (the fixnum (1- start)) 
       while (>= start 0) do
	 (sift-down-bheap q start (1- length)))
    q))

(defun extend-bheap-vector (v)
  "Double the length of vector V, i.e. create a new larger vector
and copy elements from V to the new vector.
Return the new, larger vector.

This method exists to simplify the implementation of transactional
priority queue TQUEUE: as long as bheap is concerned,
\(vector-push-extend ...) would be fine."
  (let* ((n (length v))
	 (vcopy (make-array (list (* 2 (1+ n)))
			    :element-type (array-element-type v))))
    (loop for i from 0 to (1- n) do
	 (setf (aref vcopy i) (aref v i)))
    vcopy))

;;;; ** bheap public functions

(defun empty-bheap? (q)
  (declare (type bheap q))
  "Return t if bheap Q is empty."
  (zerop (length-of q)))

(defun clear-bheap (q)
  "Remove all values from bheap Q. Return Q."
  (declare (type bheap q))
  (setf (length-of q) 0)
  q)


(defun get-bheap (q &optional default)
  "Return the first value in bheap Q without removing it, and t as multiple values.
Return (values DEFAULT nil) if Q contains no values."
  (declare (type bheap q))
  (if (empty-bheap? q)
      (values default nil)
      (values (aref (vector-of q) 0) t)))


(defun rem-bheap (q &optional default)
   "If bheap Q contains at least one value, remove the first value
and return it and t as multiple values.
Otherwise return (values DEFAULT nil)"
  (declare (type bheap q))
  (with-rw-slots (vector length) q
    (if (zerop (the fixnum length))
	(values default nil)
	(let1 value (aref vector 0)
	  (setf (aref vector 0) (aref vector (decf length)))
	  (sift-down-bheap q 0 (1- length))
	  (values value t)))))


(defun add-bheap (q value)
  "Add VALUE to bheap Q. Return VALUE."
  (declare (type bheap q))
  (with-rw-slots (vector length) q
    (declare (type fixnum length))
    (when (= length (length vector))
      (setf vector (extend-bheap-vector vector)))
    (setf (aref vector length) value)
    (sift-up-bheap q 0 length)
    (incf length)
    value))



;;;; ** Printing

(defprint-object (q bheap)
  (with-ro-slots (vector length) q
    (declare (type vector vector)
	     (type fixnum length))
    (format t "#(")
    (loop for i from 0 to (1- length) do
	 (when (= i 100)
	   (format t " ...")
	   (return))
	 (format t "~A~S" (if (zerop i) "" " ") (aref vector i)))
    (format t ")")))


;;;; ** Public methods

(defmethod empty? ((q bheap))
  "Return t if bheap Q is empty."
  (empty-bheap? q))

(defmethod empty! ((q bheap))
  "Remove all values from bheap Q. Return Q."
  (clear-bheap q))

(defmethod full? ((q bheap))
  "A bheap is never full, so this method always returns nil."
  nil)


(defmethod peek ((q bheap) &optional default)
  "Return the first value in bheap Q without removing it, and t as multiple values.
Return (values DEFAULT nil) if Q contains no values."
  (get-bheap q default))


(defmethod try-take ((q bheap))
   "If bheap S contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (multiple-value-bind (value present?) (rem-bheap q)
     (values present? value)))


(defmethod put ((q bheap) value)
  "Store VALUE in bheap Q. Return VALUE."
  (add-bheap q value))


(defmethod try-put ((q bheap) value)
  "Store VALUE in bheap Q. Return t and VALUE
This method never fails."
  (values t (add-bheap q value)))
