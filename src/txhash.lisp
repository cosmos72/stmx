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

(declaim (inline make-txlist))

(defstruct txlist
  (key   nil :type tvar)
  (value nil :type t)
  (next  nil :type (or null txlist)))



(defmacro do-txhash-entries ((entry) hash &body body)
  "Execute BODY on each TXLIST entry contained in HASH. Return NIL."
  (let ((h     (gensym "HASH-"))
	(vec   (gensym "VEC-"))
	(i     (gensym "I-"))
	(n     (gensym "N-"))
	(left  (gensym "LEFT-"))
	(next  (gensym "NEXT-"))
        (loop-name (gensym "LOOP-")))
    `(let* ((,h    (the txhash-table ,hash))
            (,vec  (the simple-vector (txhash-table-vec ,h)))
            (,n    (the fixnum (length ,vec)))
            (,left (txhash-table-count ,h)))
       (declare (fixnum ,left))
           
       (dotimes (,i ,n)
         (when (zerop ,left)
           (return))
         (loop named ,loop-name
            for ,entry = (svref ,vec ,i) then ,next
            while ,entry
            for ,next = (txlist-next ,entry)
            do
              (decf ,left)
              ,@body)))))
                


(defmacro do-txhash ((key &optional value) hash &body body)
  "Execute BODY on each KEY/VALUE contained in HASH. Return NIL."
  (let ((entry (gensym "ENTRY-")))
    `(do-txhash-entries (,entry) ,hash
       (let ((,key (txlist-key ,entry))
             ,@(when value `((,value (txlist-value ,entry)))))
         ,@body))))
                



	   
(declaim (inline txhash-mask))

(defun txhash-mask (vec-len)
  "Return the bitmask to use for hash indexes."
  (declare (type fixnum vec-len))
  (the fixnum (1- vec-len)))



(declaim (inline txhash-subscript find-txhash get-txhash))

(defun txhash-subscript (hash-code vec &optional (vec-len (length vec)))
  "Return the array subscript in HASH corresponding to TXHASH."
  (declare (type simple-vector vec)
           (type fixnum hash-code vec-len))
  (the fixnum (logand
               (txhash-mask vec-len)
               (logxor hash-code
                       (ash hash-code -10)))))
               


(defun find-txhash (hash key)
  "If KEY is present in HASH, return the TXLIST containing KEY.
Otherwise return NIL."
  (declare (type txhash-table hash)
           (type tvar key))

  (let* ((id (the fixnum (tvar-id key)))
         (vec (txhash-table-vec hash))
         (subscript (txhash-subscript id vec)))
         
    (the (or null txlist)
      (loop for entry = (svref vec subscript) then (txlist-next entry)
         while entry do
           (when (eq key (txlist-key entry))
             (return entry))))))


(defun get-txhash (hash key &optional default)
  "If KEY is associated to VALUE in HASH, return (values VALUE t)
Otherwise return (values DEFAULT nil)."
  (declare (type txhash-table hash)
           (type tvar key))
  (if-bind entry (find-txhash hash key)
    (values (txlist-value entry) t)
    (values default nil)))

    
  
(defun rehash-txhash (hash)
  (declare (type txhash-table hash))

  (let* ((vec1 (the simple-vector (txhash-table-vec hash)))
         (n2   (the fixnum (ash (length vec1) 1)))
         (vec2 (the simple-vector (make-array n2 :initial-element nil))))
    
    (do-txhash-entries (entry) hash
      (let* ((key (txlist-key entry))
             (id (tvar-id key))
             (subscript (txhash-subscript id vec2 n2))
             (head (svref vec2 subscript)))
        
        (setf (txlist-next entry) head
              (svref vec2 subscript) entry)))

    (setf (txhash-table-vec hash) vec2)))
        
        
      
(declaim (inline new-txlist-from-pool))
(defun new-txlist-from-pool (hash txlist-next key value)
  (declare (type txhash-table hash)
           (type (or null txlist) txlist-next))

  (the txlist
    (if-bind entry (txhash-table-pool hash)
      (progn
        (setf (txhash-table-pool hash) (txlist-next entry)
              (txlist-key   entry) key
              (txlist-value entry) value
              (txlist-next  entry) txlist-next)
        entry)
      (make-txlist :key key :value value :next txlist-next))))


(defun set-txhash (hash key value)
  "Add KEY to HASH, associating it to VALUE. Return VALUE."
  (declare (type txhash-table hash)
           (type tvar key))

  (let* ((id (tvar-id key))
         (vec (txhash-table-vec hash))
         (subscript (txhash-subscript id vec))
         (head (svref vec subscript)))

    (loop for entry = head then (txlist-next entry)
       while entry do
         (when (eq key (txlist-key entry))
           (return-from set-txhash (setf (txlist-value entry) value))))

    (setf (svref vec subscript) (new-txlist-from-pool hash head key value))

    (when (> (the fixnum (incf (txhash-table-count hash)))
             (length vec))
      (rehash-txhash hash))

    value))


(let ((dummy-tvar (make-tvar :id -1)))
  (defun add-txlist-to-pool (hash txlist)
    (declare (type txhash-table hash)
             (type txlist txlist))
    (loop for entry = txlist then next
       for next = (txlist-next entry)
       do
	 (setf (txlist-key   entry) dummy-tvar
	       (txlist-value entry) nil)
	 (unless next
	   (setf (txlist-next entry) (txhash-table-pool hash))
	   (return)))
    (setf (txhash-table-pool hash) txlist)))
            
               

(declaim (type fixnum +txhash-threshold-capacity+))
(defconstant +txhash-threshold-capacity+ 64)

(defun clear-txhash (hash)
  "Remove all keys and values from HASH. Return HASH."
  (declare (type txhash-table hash))

  (unless (zerop (txhash-table-count hash))
    (let* ((vec (txhash-table-vec hash))
           (n (length vec)))
      (if (<= n +txhash-threshold-capacity+)
          (loop for i from 0 to (1- n)
             for txlist = (svref vec i)
             when txlist do
	       (add-txlist-to-pool hash txlist)
               (setf (svref vec i) nil))
          (setf (txhash-table-vec hash)
                (make-array +txhash-default-capacity+
                            :initial-element nil))))
    (setf (txhash-table-count hash) 0))
  hash)




       
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

(defmethod print-object ((obj txlist) stream)
  (format stream "#S(~S " 'txlist)
  (loop for entry = obj then (txlist-next entry)
     while entry do
       (unless (eq entry obj)
         (format stream "~&"))
       (format stream "~S ~S ~S ~S"
               :key (txlist-key entry) :value (txlist-value entry)))
  (format stream ")"))


