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
   ((original            :type hash-table           :accessor original-of)
    (delta :initform nil :type (or null hash-table) :accessor delta-of)
    (count :initform 0   :type fixnum               :accessor count-of))))


(defmethod initialize-instance :after
    ((instance thash-table) &rest other-keys &key (test 'eql)  &allow-other-keys)
  "Initialize a new transactional hash-table. Accepts the same arguments as MAKE-HASH-TABLE,
including any non-standard arguments supported by MAKE-HASH-TABLE implementation."

  (log:trace "test = ~A other-keys = ( ~{~A ~})" test other-keys)
  (setf (original-of instance)
        (apply #'make-hash-table :test test other-keys)))


(defun thash-count (thash)
  "Return the number of KEY/VALUE entries in THASH."
  (declare (type thash-table thash))
  (the fixnum (count-of thash)))


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
   (gethash key (original-of thash) default)))


(transaction
 (defun (setf get-thash) (value key thash)
   "Store the VALUE associated to KEY in THASH. Return VALUE."
   (declare (type thash-table thash))

   (multiple-value-bind (old-value present?) (get-thash key thash)
     (declare (ignore old-value))
     (unless present?
       (incf (the fixnum (count-of thash)))))

   (let1 delta (ensure-thash-delta thash)
     (setf (gethash key delta) value))))
     

(transaction
 (defun rem-thash (key thash)
   "Remove the VALUE associated to KEY in THASH.
Return T if KEY was found in THASH, otherwise return NIL."
   (declare (type thash-table thash))

   (multiple-value-bind (orig-value present?) (get-thash key thash)
     (declare (ignore orig-value))
     (if present?
         (let ((delta (ensure-thash-delta thash)))
           (decf (the fixnum (count-of thash)))
           (setf (gethash key delta) *thash-removed-entry*)
           t)
         nil)))) ;; key not present in THASH


(transaction
 (defun clear-thash (thash)
   "Remove all KEYS and VALUES in THASH. Return THASH."
   (declare (type thash-table thash))

   (setf (count-of thash) (the fixnum 0))

   (with-ro-slots (original delta) thash
     (when (zerop (hash-table-count original))
       (when delta
         (clrhash delta))
       (return-from clear-thash thash))

     (let ((delta (ensure-thash-delta thash)))
       (clrhash delta)
       (do-hash (key) original
         (setf (gethash key delta) *thash-removed-entry*))))))



(defun map-thash (thash func)
  "Invoke FUNC on each key/value pair contained in transactional hash table THASH.
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
  (let* ((dummy (gensym))
         (func-body `(lambda (,key ,(or value dummy))
                       ,@(unless value `((declare (ignore ,dummy))))
                       ,@body)))
    `(map-thash ,thash ,func-body)))
        



(defun ensure-thash-delta (thash)
  "Create and set slot DELTA if nil. Return slot value."
   (declare (type thash-table thash))

   (with-rw-slots (delta) thash
     (aif delta
          it
          (progn
            (before-commit (normalize-thash thash))
            (setf delta (make-hash-table :test (hash-table-test (original-of thash))))))))



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

      (with-rw-slots (original delta) thash
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

