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

;;;; ** Implementation classes

(declaim (type integer *tlog-id-counter*))
(defvar *tlog-id-counter* 0)

(defclass tlog ()
  ((reads  :accessor reads-of
           :initarg :reads
           :initform (make-hash-table :test 'eq)
           :type hash-table
           :documentation "TVARs read during transaction, mapped to their read value")
   (writes :accessor writes-of
           :initarg :writes
           :initform (make-hash-table :test 'eq)
           :type hash-table
           :documentation "TVARs written during transaction, mapped to their new values")
   (parent :accessor parent-of
           :initarg :parent
           :initform nil
           :type (or null tlog)
           :documentation "Parent of this TLOG. Used for nested transactions")
   (lock   :accessor lock-of
           :initform nil)
   (semaphore :accessor semaphore-of
              :initform nil)
   (prevent-sleep :accessor prevent-sleep-of
                  :initform nil
                  :type boolean
                  :documentation "Flag to prevent TLOGs from sleeping.
Set by TVARs when they change")
   (before-commit :accessor before-commit-of
                  :initform nil
                  :type (or null vector)
                  :documentation "functions to call immediately before committing TLOG.")
   (after-commit :accessor after-commit-of
                 :initform nil
                 :type (or null vector)
                 :documentation "functions to call immediately after committing TLOG.")
   (id :reader id-of
       :initform (incf *tlog-id-counter*)
       :type integer))

  (:documentation "A transaction log (TLOG) is a record of the reads and writes
to transactional memory performed during a transaction.

Transaction logs are automatically populated by reading and writing
transactional objects (TOBJs) or transactional variables (TVARs),
and are later committed to memory if the transaction completes successfully."))



(defvar +unbound+ (gensym "UNBOUND-"))

(defclass tvar ()
  ((value :accessor raw-value-of
          :initarg :value
          :initform +unbound+)
   (lock :accessor lock-of
         :initform (make-lock "TVAR"))
   (waiting :accessor waiting-for
            :initform nil
            :type (or null hash-table))
   (waiting-lock :accessor waiting-lock-of
                 :initform (make-lock "WAITING-LOCK")))

  (:documentation "A transactional variable (TVAR) is the smallest unit
of transactional memory.
It contains a single value that can be read or written during a transaction
using ($ tvar) and (SETF ($ tvar) value).

TVARs are seldom used directly, since transactional objects (TOBJs) wrap them
with a more intuitive and powerful interface: you can read and write normally
the slots of a transactional object (with slot-value, accessors ...),
and behind the scenes the slots will be stored in transactional memory implemented by TVARs."))



;;;; ** Flags to control the behaviour of TLOGs and TOBJs

(declaim (type boolean *record-to-tlogs*))
(defvar *record-to-tlogs* nil
  "A flag indicating if transactions are being recorded to TLOGs or not.

Recording is normally enabled during transactions,
while it is normally disabled in these cases:
- outside transactions
- when initializing TOBJs slots
- during some MOP calls (slot-value-using-class etc.) that implement
  access to TOBJs slots.")

(declaim (inline recording?))
(defun recording? ()
  "Return true if transactions are being recorded to TLOGs."
  *record-to-tlogs*)

(defmacro with-recording (&body body)
  "Enable recording of reads and writes to TLOGs while executing BODY."
  `(let1 *record-to-tlogs* t
     ,@body))


(declaim (type boolean *hide-tvars*))
(defvar *hide-tvars* t
  "A boolean controlling the behaviour of the function (slot-value)
and of slot accessors for TOBJs slots:
- if false, (slot-value) and accessors will get or set the actual TVAR stored
  inside the slot.
- if true, (slot-value) and accessors will get or set the value stored inside
  the slot's TVAR.

This flag is almost always true; it is temporarily set to false during some MOP
calls (slot-value-using-class ...) that implement access to TOBJs slots.")

(defun hide-tvars? ()
  *hide-tvars*)

(defmacro without-recording-with-show-tvars (&body body)
  "Disable recording of transactions to TLOGs and disable hiding TVARs
inside TOBJs slots while executing BODY."
  `(let ((*record-to-tlogs* nil)
         (*hide-tvars* nil))
     ,@body))



;;;; ** Current transaction log

(declaim (type (or null tlog) *tlog*))
(defvar *tlog* nil
  "The current transaction log.")

(declaim (inline current-tlog))
(defun current-tlog ()
  "Return the current transaction log"
  *tlog*)

(defmacro with-tlog (log &body body)
  "Use LOG as the current transaction log while executing BODY."
  `(let1 *tlog* ,log
     ,@body))

(defmacro with-recording-to-tlog (log &body body)
  "Use LOG as the current transaction log and enable recording of transactions
to TLOGs while executing BODY."
  `(with-tlog ,log
     (with-recording
       ,@body)))


(eval-always
  (unless (assoc '*tlog* bt:*default-special-bindings*)
    (push '(*tlog* . nil) bt:*default-special-bindings*)))
