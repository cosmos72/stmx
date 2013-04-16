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


(in-package :stmx.lang)

;;;; * Hash-table utilities


#+never
(eval-always
 (defstruct hash-counter
   "(stmx.example1::dining-philosophers 4 1000000) on average
does 4.4 million iterations and causes the following
hash table operations for EACH iteration:
  15.0  get
   8.0  set
  11.4  iterations inside do-hash"

  (get 0 :type sb-vm:word)
  (set 0 :type sb-vm:word)
  (rem 0 :type sb-vm:word)
  (clear 0 :type sb-vm:word)
  (loop 0 :type sb-vm:word))


 (defvar *hash-counter* (make-hash-counter))

 (defmacro incf-hash-counter (which)
   (let1 accessor (intern (concatenate 'string "HASH-COUNTER-" (symbol-name which)) 'stmx.lang)
     `(progn
        (sb-ext:atomic-incf (,accessor *hash-counter*))
        nil))))

#-always
(defmacro incf-hash-counter (which)
  (declare (ignore which))
  nil)


(defmacro do-hash ((key &optional value) hash &body body)
  "Execute body on each key/value pair contained in hash table"
  `(loop for ,key being each hash-key in ,hash
      ,@(when value `(using (hash-value ,value)))
      do (progn
           (incf-hash-counter loop)
           ,@body)))
        
(declaim (inline get-hash))
(defun get-hash (hash key)
  "Same as (gethash key hash), only with reversed arguments."
  (declare (type hash-table hash))
  (incf-hash-counter get)
  (gethash key hash))


(declaim (inline set-hash))
(defun set-hash (hash key value)
  "Shortcut for (setf (gethash key hash) value)"
  (declare (type hash-table hash))
  (incf-hash-counter set)
  (setf (gethash key hash) value))

(declaim (inline (setf get-hash)))
(defun (setf get-hash) (value hash key)
  "Same as (setf (gethash key hash) value), only with reversed key and hash arguments."
  (declare (type hash-table hash))
  (setf (gethash key hash) value))



(declaim (inline rem-hash))
(defun rem-hash (hash key)
  "Same as (remhash key hash), only with reversed arguments."
  (declare (type hash-table hash))
  (incf-hash-counter rem)
  (remhash key hash))


(declaim (inline clear-hash))
(defun clear-hash (hash)
  "Same as (clrhash hash)."
  (declare (type hash-table hash))
  (incf-hash-counter clear)
  (clrhash hash))



(defun hash-table-keys (src &optional to-list)
  "Return a list containing the keys in hash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type hash-table src)
           (type list to-list))
  (do-hash (key) src
    (push key to-list))
  to-list)


(defun hash-table-values (src &optional to-list)
  "Return a list containing the values in hash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type hash-table src)
           (type list to-list))
  (do-hash (key value) src
    (push value to-list))
  to-list)


(defun hash-table-pairs (src &optional to-alist)
  "Return an alist containing a (key . value) pair for each entry
in hash-table SRC.
If TO-ALIST is not nil, it will be appended to the returned alist.
TO-ALIST contents is not destructively modified."
  (declare (type hash-table src)
           (type list to-alist))
  (do-hash (key value) src
    (push (cons key value) to-alist))
  to-alist)
  
  
(defun copy-hash-table (dst src)
  "Copy all key/value pairs from hash-table SRC into hash-table DST.
Other keys (and their values) present in DST but not in SRC
are not modified. Return DST."
  (declare (type hash-table dst src))
  (do-hash (key value) src
    (set-hash dst key value))
  dst)




(defun merge-hash-tables (dst src)
  "Copy hash-table SRC into hash-table DST.

Return t if SRC and DST are compatible,
i.e. if they contain eq values for the keys common to both,
otherwise return nil.
\(in the latter case, the merge will not be completed)."

  (declare (type hash-table src dst))
  (do-hash (var val1) src
    (multiple-value-bind (val2 present2?) (get-hash dst var)
      (when (and present2? (not (eq val1 val2)))
        (return-from merge-hash-tables nil))
      (set-hash dst var val1)))
  t)
