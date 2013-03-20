;; -*- lisp -*-

(in-package :stmx)

;;;; * Hash-table utilities

(defmacro dohash (hash key value &body body)
  "Execute body on each key/value pair contained in hash table"
  `(loop for ,key being each hash-key in ,hash
        using (hash-value ,value)
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


(defun reset-hash-table (hash &key defaults)
  "Clear HASH hash table. If DEFAULTS is not nil, copy it into HASH.

In both cases, return HASH."

  (declare (type hash-table hash)
           (type (or null hash-table) defaults))
  (clrhash hash)
  (when defaults
    (dohash defaults key value
      (set-hash hash key value)))
  hash)


(defun copy-hash-table (hash)
  "Create and return a new hash-table containing the same keys and values as HASH."
  (declare (type hash-table hash))
  (let1 copy (make-hash-table :test (hash-table-test hash)
                              :size (hash-table-size hash))
    (dohash hash key value
      (set-hash copy key value))
    copy))




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



;; Copyright (c) 2013, Massimiliano Ghilardi
;; This file is part of STMX.
;;
;; STMX is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; STMX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with STMX. If not, see <http://www.gnu.org/licenses/>.
