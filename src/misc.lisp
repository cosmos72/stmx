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

;;;; * Hash-table utilities

(defmacro dohash ((key &optional value) hash &body body)
  "Execute body on each key/value pair contained in hash table"
  `(loop for ,key being each hash-key in ,hash
      ,@(when value `(using (hash-value ,value)))
      do (progn ,@body)))
        
(declaim (inline get-hash set-hash))

(defun get-hash (hash key)
  "Same as (gethash key hash), only with reversed arguments."
  (declare (type hash-table hash))
  (gethash key hash))

(defun set-hash (hash key value)
  "Shortcut for (setf (gethash key hash) value)"
  (declare (type hash-table hash))
  (setf (gethash key hash) value))


(defun copy-hash-table (dst src)
  "Copy all key/value pairs from hash-table SRC into hash-table DST.
Other keys (and their values) present in DST but not in SRC
are not modified. Return DST."

  (declare (type hash-table dst src))
  (dohash (key value) src
    (set-hash dst key value))
  dst)


(defun reset-hash-table (hash &key defaults)
  "Clear hash-table HASH. If DEFAULTS is not nil, copy it into HASH.
Return HASH."

  (declare (type hash-table hash)
           (type (or null hash-table) defaults))
  (clrhash hash)
  (when defaults
    (copy-hash-table hash defaults))
  hash)


(defun clone-hash-table (hash)
  "Create and return a new hash-table containing the same keys and values as HASH.
The new hash-table inherits :test from HASH."
  (declare (type hash-table hash))
  (let1 copy (make-hash-table :test (hash-table-test hash)
                              :size (hash-table-size hash))
    (copy-hash-table copy hash)))


(defun merge-hash-tables (dst src)
  "Copy hash-table SRC into hash-table DST.

Return T if SRC and DST are compatible, i.e. if they contain the same values
for the keys common to both, otherwise return NIL
\(in the latter case, the merge will not be completed)."

  (declare (type hash-table src dst))
  (dohash (var val1) src
    (multiple-value-bind (val2 present2?) (gethash var dst)
      (when (and present2? (not (eq val1 val2)))
        (return-from merge-hash-tables nil))
      (setf (gethash var dst) val1)))
  t)




;;;; * Printing utilities

(defgeneric id-of (obj))
(defgeneric (setf id-of) (value obj))

(defun compute-string-of (obj)
  (handler-case
      (format nil "~A" obj)
    (t ()
      (handler-case
          (format nil "~S" obj)
        (t ()
          "<error printing object>")))))

(defun compute-id-of (obj)
  (declare (type t obj))
  (let* ((str (the string (compute-string-of obj)))
         (beg (position #\{ str))
         (end (position #\} str)))
    (the string
      (if (and beg end)
          (subseq str (1+ beg) end)
          str))))

(let1 ids (make-hash-table :test 'eq :size 100 :weakness :key)
  (defmethod id-of (obj)
    (the string
      (or
       (get-hash ids obj)
       (set-hash ids obj (compute-id-of obj)))))

  (defmethod (setf id-of) (value obj)
    (set-hash ids obj (format nil "~A" value))))


(declaim (inline ~ (setf ~)))

(defun ~ (obj)
  (id-of obj))

(defun (setf ~) (value obj)
  (setf (id-of obj) value))
