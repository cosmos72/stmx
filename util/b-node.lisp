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


(in-package :stmx.util)

;;;; ** b-node: node of non-transactional b-tree

(deftype b-node () 'simple-vector)

(defconstant +b-node-maxlen+ (truncate (1- array-dimension-limit) 3))

(deftype b-node-len () `(integer 0 ,+b-node-maxlen+))

(deftype b-node-pred () '(function (t t) (values boolean &optional)))

;; b-node elements are organized as follows:
;;
;; 0) MIN = index of first key, must be >= 1
;; 1) MAX = 1+ index of last key, must be <= (* 3 array-length)
;; ...
;; 3*MIN-1) -1-th child pointer
;; 3*MIN+0) 0-th key
;; 3*MIN+1) 0-th value (payload of first key)
;; 3*MIN+2) 0-th child pointer
;; 3*MIN+3) 1-st key
;; 3*MIN+4) 1-st value (payload of second key)
;; 3*MIN+5) 1-st child pointer
;; ...
;; 3*MIN+3*k+0) k-th key
;; 3*MIN+3*k+1) k-th value (payload of k-th key)
;; 3*MIN+3*k-2) k-th child pointer
;; ...
;; 3*MAX-3) last key
;; 3*MAX-2) last value (payload of last key)
;; 3*MAX-1) last child pointer

(declaim (inline b-node-min b-node-max b-node-count b-node-size
		 (setf b-node-min) (setf b-node-max)))

(defun b-node-min (node)
  (declare (type b-node node))
  (the b-node-len (svref node 0)))

(defun (setf b-node-min) (value node)
  (declare (type b-node-len value)
	   (b-node node))
  (setf (svref node 0) value))


(defun b-node-max (node)
  (declare (type b-node node))
  (the b-node-len (svref node 1)))

(defun (setf b-node-max) (value node)
  (declare (type b-node-len value)
	   (b-node node))
  (setf (svref node 1) value))


(defun b-node-count (node)
  "Return number of keys currently stored in NODE"
  (declare (type b-node node))
  (the b-node-len
       (- (b-node-max node)
	  (b-node-min node))))

(defun b-node-size (node)
  "Return maximum number of keys storable in NODE"
  (declare (type b-node node))
  (the b-node-len
       (1- (truncate (length node) 3))))


(declaim (inline b-node-key b-node-value b-node-child
		 (setf b-node-key) (setf b-node-value) (setf b-node-child)))

(defun b-node-key (node index)
  (declare (type b-node node)
	   (type b-node-len index))
  (svref node (* 3 index)))

(defun (setf b-node-key) (key node index)
  (declare (type b-node node)
	   (type b-node-len index))
  (setf (svref node (* 3 index)) key))


(defun b-node-value (node index)
  (declare (type b-node node)
	   (type b-node-len index))
  (svref node (1+ (* 3 index))))

(defun (setf b-node-value) (value node index)
  (declare (type b-node node)
	   (type b-node-len index))
  (setf (svref node (1+ (* 3 index))) value))


(defun b-node-child (node index)
  (declare (type b-node node)
	   (type b-node-len index))
  (svref node (+ 2 (* 3 index))))

(defun (setf b-node-child) (child node index)
  (declare (type b-node node)
	   (type b-node-len index)
	   (type (or null b-node) child))
  (setf (svref node (+ 2 (* 3 index))) child))



(defun make-b-node (n-keys)
  (declare (type (and (integer 2) b-node-len) n-keys))
  (let* ((length (* (1+ n-keys) 3))
	 (mid-len (ash (1+ n-keys) -1))
	 (node (make-array length :initial-element nil)))
    (setf (b-node-min node) mid-len
	  (b-node-max node) mid-len)
    node))

(declaim (ftype (function (b-node function t) b-node-len)
		b-node-lower-bound))

(defun b-node-lower-bound (node pred key)
  "Search for KEY in NODE.
Return index of the smallest key in NODE that is not lesser than KEY.
\(PRED key1 key2) is used to determine which key is \"lesser\".

Note: if all keys in NODE are lesser than KEY, returned index
will point immediately after the last element in NODE."
  (declare (type b-node node)
	   (type function pred))
  (let ((lo (b-node-min node))
	(hi (b-node-max node)))
    (loop
       (let ((delta (the b-node-len (- hi lo))))
	 (when (<= delta 0)
	   (return lo))
	 (let* ((mid  (the b-node-len (+ lo (truncate delta 2))))
		(kmid (b-node-key node mid)))
	   ;; (format t "lo = ~S, hi = ~S, comparing node[~S] = ~S with key ~S~%" lo hi mid kmid key)
	   (if (funcall pred kmid key)
	       (setf lo (1+ mid))
	       (setf hi mid)))))))


(defun b-node-get (node pred key &optional default)
  "Search for KEY in NODE.
If found, return value associated to it and T as multiple values,
otherwise return (values DEFAULT NIL)."
  (declare (type b-node node)
	   (type function pred))

  (let ((index (b-node-lower-bound node pred key))
	(end (b-node-max node)))
    (when (< index end)
      (let ((kmid (b-node-key node index)))
	(unless (funcall pred key kmid)
	  (return-from b-node-get
	    (values (b-node-value node index) t)))))

    (values default nil)))


(defun b-node-move (node to-index from-index n-keys)
  (declare (type b-node node)
	   (type b-node-len to-index from-index n-keys))

  (let ((to-index-end (the b-node-len (+ to-index n-keys))))
    (if (< to-index from-index)
	(loop while (< to-index to-index-end) do
	     (
	     
	       
  

(defun b-node-add (node pred key value)
  "Insert KEY and VALUE into NODE. Return T if successful,
or NIL if NODE is already full."
  (declare (type b-node node)
	   (type function pred))
  
  (when (>= (b-node-count node)
	    (b-node-size node))
    (return-from b-node-add nil))

  (let ((index (b-node-lower-bound node pred key))
	(min (b-node-min node))
	(max (b-node-max node)))
    
	
    
	
