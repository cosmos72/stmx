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

;;;; ** Transactional hash table

(defvar *thash-removed-entry* (gensym "REMOVED"))

(transactional
 (defclass thash-table ()
   ((original            :type hash-table           :accessor original-hash)
    (delta :initform nil :type (or null hash-table) :accessor delta-hash))))


(defmethod initialize-instance :after
    ((instance thash-table) &rest other-keys &key (test 'eql)  &allow-other-keys)
  "Initialize a new transactional hash-table. Accepts the same arguments as MAKE-HASH-TABLE,
including any non-standard arguments supported by MAKE-HASH-TABLE implementation."

  (log:trace "test = ~A other-keys = ( ~{~A ~})" test other-keys)
  (setf (original-hash instance)
        (apply #'make-hash-table :test test other-keys)))




(transaction
 (defun get-thash (key thash &optional default)
   "Find KEY in THASH and return its value and T as multiple values.
If THASH does not contain KEY, return (values DEFAULT NIL)."
   (declare (type thash-table thash))

   (with-ro-slots (delta) thash
     (when delta
       (multiple-value-bind (value present?) (gethash key delta)
         (when present?
           (return-from get-thash
             (if (eq value *thash-removed-entry*)
                 (values default nil)   ;; key was removed
                 (values value t))))))) ;; key was changed
               
   ;; forward to original hash table
   (gethash key (original-hash thash) default)))


(transaction
 (defun (setf get-thash) (value key thash)
   "Store the VALUE associated to KEY in THASH. Return VALUE."
   (declare (type thash-table thash))

   (let1 delta (ensure-thash-delta thash)
     (setf (gethash key delta) value))))
     

(transaction
 (defun rem-thash (key thash)
   "Remove the VALUE associated to KEY in THASH.
Return T if KEY was found in THASH, otherwise return NIL."
   (declare (type thash-table thash))

   (multiple-value-bind (orig-value present?) (gethash key (original-hash thash))
     (declare (ignore orig-value))
     (if present?
         (let* ((delta (ensure-thash-delta thash))
                (delta-value (gethash key delta)))

           (if (eq delta-value *thash-removed-entry*)
               nil ;; key was already removed
               (progn ;; set key to removed in DELTA
                 (setf (gethash key delta) *thash-removed-entry*)
                 t)))
         nil)))) ;; key not present in ORIGINAL


(defun map-thash (thash func)
  "Execute FUNC on each key/value pair contained in transactional hash table THASH.
FUNC must be a function accepting two arguments: key and value."
  (with-ro-slots (original delta) thash
    (if delta
        (progn
          (do-hash (key value) delta
            (unless (eq *thash-removed-entry* value)
              (funcall func key value)))
          (do-hash (key value) original
            (multiple-value-bind (delta-value? delta-present?) (gethash key delta)
              (declare (ignore delta-value?))
              ;; skip keys present in delta, especially removed keys
              (unless delta-present?
                (funcall func key value)))))
        ;; easy, delta is nil
        (do-hash (key value) original
          (funcall func key value)))))


(defmacro do-thash ((key &optional value) thash &body body)
  "Execute body on each key/value pair contained in transactional hash table THASH"
  (let1 value-name (if value value (gensym))
    `(map-thash ,thash (lambda (,key ,value-name) ,@body))))
        



(defun ensure-thash-delta (thash)
  "Create and set slot DELTA if nil. Return slot value."
   (declare (type thash-table thash))

   (with-slots (delta) thash
     (if delta
         delta
         (progn
           (before-commit (normalize-thash thash))
           (setf delta (make-hash-table :test (hash-table-test (original-hash thash))))))))



(defun normalize-thash (thash)
  "Apply the changes stored in THASH slot DELTA.

Implementation notes:
- when slot DELTA is created, (before-commit (normalize-thash thash))
  must be called in order for this function to be invoked just before
  the transaction commits.

- hash table in slot ORIGINAL must not be modified, yet slot ORIGINAL must
  atomically be set to the new value. The only practical solution is to
  populate a new hash-table (we reuse DELTA for that) then change
  the slot ORIGINAL to point to the new hash-table, while keeping intact
  the old hash-table contents."

  (with-ro-slots (original delta) thash
    (when delta
      (log:trace "before: ~A" (print-object-contents nil thash))
      (do-hash (key orig-value) original
        (multiple-value-bind (delta-value delta-present?) (gethash key delta)
          (if delta-present?
              ;; remove DELTA entries marked *thash-removed-entry*,
              ;; ignore orig-value since DELTA contains the updated value
              (when (eq delta-value *thash-removed-entry*)
                (remhash key delta))
              ;; no entry in DELTA, copy it from ORIGINAL
              (setf (gethash key delta) orig-value))))

      (with-slots (original delta) thash
        (setf original delta)
        (setf delta nil))

      (log:trace "after:  ~A" (print-object-contents nil thash)))))

                




(defmethod print-object-contents (stream (obj thash-table))
  (format stream "{original=")
  (with-ro-slots (original delta) obj
    (print-object-contents stream original)
    (format stream ", delta=")
    (if delta
        (print-object-contents stream delta)
        (format stream "nil")))
  (format stream "}"))

