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


(in-package :stmx)

;;;; * Hash-table specialized for TVAR keys

(declaim (inline txhash-mask))

(defun txhash-mask (vec)
  "Return the bitmask to use for hash indexes."
  (declare (type simple-vector vec))
  (the fixnum (1- (length vec))))


(declaim (inline make-triplet))

(defstruct triplet
  (key   nil :type tvar)
  (value nil :type t)
  (next  nil :type (or null triplet)))


(declaim (inline txhash-subscript get-txhash))

(defun txhash-subscript (hash vec hash-code)
  "Return the array subscript in HASH corresponding to TXHASH."
  (declare (type txhash-table hash)
           (type simple-vector vec)
           (type fixnum hash-code)
           (ignore hash))
  (the fixnum (logand
               (txhash-mask vec)
               (logxor hash-code
                       (ash hash-code -10)))))
               




(defun get-txhash (hash key &optional default)
  "If KEY is associated to VALUE in HASH, return (values VALUE t)
Otherwise return (values DEFAULT nil)."
  (declare (type txhash-table hash)
           (type tvar key))
  (let* ((id (the fixnum (tvar-id key)))
         (vec (txhash-table-vec hash))
         (subscript (txhash-subscript hash vec id)))
         
    (loop for entry = (svref vec subscript) then (triplet-next entry)
       while entry do
         (when (eq key (triplet-key entry))
           (return-from get-txhash (values (triplet-value entry) t)))))

  (values default nil))
    

(defun rehash-txhash (hash)
  (declare (type txhash-table hash)
	   (ignore hash))
  (error "~A not yet implemented" 'rehash-txhash))

(defun set-txhash (hash key value)
  "Add KEY to HASH, associating it to VALUE. Return VALUE."
  (declare (type txhash-table hash)
           (type tvar key))

  (let* ((id (tvar-id key))
         (vec (txhash-table-vec hash))
         (subscript (txhash-subscript hash vec id))
         (head (svref vec subscript)))

    (loop for entry = head then (triplet-next entry)
       while entry do
         (when (eq key (triplet-key entry))
           (return-from set-txhash (setf (triplet-value entry) value))))

    (let* ((pool (txhash-table-pool hash))
           (triplet (if pool
                        (progn
                            (decf (txhash-table-pool-size hash))
                            (setf (txhash-table-pool hash) (triplet-next pool)
                                  (triplet-key   pool) key
                                  (triplet-value pool) value
                                  (triplet-next  pool) head)
                            pool)
                        (make-triplet :key key :value value :next head))))
      
      (setf (svref vec subscript) triplet))

    (when (> (the fixnum (incf (txhash-table-count hash)))
             (length vec))
      (rehash-txhash hash))

    value))


(let ((dummy-tvar (make-tvar :id -1)))
  (defun add-triplets-to-pool (hash triplets)
    (declare (type txhash-table hash)
             (type triplet triplets))
    (let ((pool      (txhash-table-pool      hash))
          (pool-size (txhash-table-pool-size hash)))
      (declare (type fixnum pool-size))
      (loop for entry = triplets then next
         while entry
         for next = (triplet-next entry)
         do
           (setf (triplet-key   entry) dummy-tvar
                 (triplet-value entry) nil)
           (incf pool-size)
           (unless next
             (setf (triplet-next entry) pool)
             (return)))
      (setf (txhash-table-pool      hash) triplets
            (txhash-table-pool-size hash) pool-size))))
            
               

       
  
(defun clear-txhash (hash &key (min-capacity
                                +txhash-default-capacity+))
  "Remove all keys and values from HASH. Return HASH."
  (declare (type txhash-table hash)
	   (type txhash-capacity min-capacity))
  (let* ((vec (txhash-table-vec hash))
         (n (length vec)))
    (if (<= min-capacity n (ash min-capacity 1))
        (loop for i from 0 to (1- n)
           for triplet = (svref vec i)
           when triplet do
             (when (< (txhash-table-pool-size hash) n)
               (add-triplets-to-pool hash triplet))
             (setf (svref vec i) nil))
        (setf (txhash-table-vec hash)
	      (make-array min-capacity
			  :initial-element nil))))
  (setf (txhash-table-count hash) 0)
  hash)

(defmacro do-txhash ((key &optional value) hash &body body)
  "Execute BODY on each KEY/VALUE contained in HASH. Return NIL."
  (let ((h    (gensym "HASH-"))
	(vec  (gensym "VEC-"))
	(i    (gensym "I-"))
	(n    (gensym "N-"))
	(left (gensym "SEEN-"))
	(entry (gensym "ENTRY-")))
    `(block nil
       (let* ((,h (the txhash-table ,hash))
	      (,vec (the simple-vector (txhash-table-vec ,h)))
	      (,n (the fixnum (length ,vec)))
	      (,left ,n))
	 (declare (fixnum ,left))
	 (dotimes (,i ,n)
	   (loop for ,entry = (svref ,vec ,i) then (triplet-next ,entry)
	      while ,entry
	      for ,key = (triplet-key ,entry)
		,@(when value 
			`(for ,value = (triplet-value ,entry)))
	      do
		(progn
		  (decf ,left)
		  ,@body))
	   (unless (plusp ,left)
	     (return)))))))
		

	   
       
(defun copy-txhash-table-into (dst src)
  "Clear DST, then copy SRC contents into it. Return NIL."
  (declare (type txhash-table src dst))
  (clear-txhash dst)
  (do-txhash (k v) src
    (set-txhash dst k v)))



(defun merge-txhash-tables (dst src)
  "Copy hash-table SRC into hash-table DST.

Return t if SRC and DST are compatible,
i.e. if they contain eq values for the keys common to both,
otherwise return nil.
\(in the latter case, the merge will not be completed)."

  (declare (type txhash-table src dst))
  (do-txhash (var val1) src
    (multiple-value-bind (val2 present2?) (get-txhash dst var)
      (when (and present2? (not (eq val1 val2)))
        (return-from merge-txhash-tables nil))
      (set-txhash dst var val1)))
  t)

  
;; (defvar ht (make-txhash-table))
;; (declaim (type fixnum n))
;; (defvar n 0)
;;  3.0 nanoseconds per (get-txhash ht v) if v not present
;;  3.5 nanoseconds per (get-txhash ht v) if v is  present
;;  6.3 nanoseconds per (the fixnum (incf n (get-txhash ht v)))
;;
;;  8.8 nanoseconds per (set-txhash ht v 1), checking for rehash
;; 11.4 nanoseconds per (the fixnum (incf n (set-txhash ht v 1))), checking for rehash
;;
;; 12.1 nanoseconds per (clear-txhash ht), 16 elements
;; 28.0 nanoseconds per (clear-txhash ht), 32 elements
;; 42.7 nanoseconds per (clear-txhash ht), 64 elements
;; 75.0 nanoseconds per (clear-txhash ht), 128 elements
;; 30.4 nanoseconds per (clear-txhash ht), creates new 16-element array
;; 56.0 nanoseconds per (clear-txhash ht), creates new 32-element array
;;107   nanoseconds per (clear-txhash ht), creates new 64-element array





;; (defvar h (make-hash-table :test 'eq))
;; (declaim (type fixnum n))
;; (defvar n 0)
;;  13.5 nanoseconds per (the fixnum (incf n (gethash v h))
;;   7.5 nanoseconds per (setf (gethash v h) 1))
;;   9.9 nanoseconds per (the fixnum (incf n (setf (gethash v h) 1)))




;;  9.0 nanoseconds per (clrhash h) (incf n (hash-table-count h)), empty hash table
;;380   nanoseconds per (incf n (hash-table-count (make-hash-table))))

(defmethod print-object ((obj triplet) stream)
  (format stream "#S(~S " 'triplet)
  (loop for entry = obj then (triplet-next entry)
     while entry do
       (unless (eq entry obj)
         (format stream "~&"))
       (format stream "~S ~S ~S ~S"
               :key (triplet-key entry) :value (triplet-value entry)))
  (format stream ")"))


