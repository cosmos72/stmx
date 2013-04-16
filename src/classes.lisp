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

;;;; ** Counters

(declaim (type fixnum *tlog-id*))
(defvar *tlog-id* 0)

(declaim (type fixnum *tvar-id*))
(defvar *tvar-id* 0)

(declaim (inline get-next-id))
(eval-always
  (if (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
    (defun get-next-id (id)
      (declare (type fixnum id))
      (the fixnum
        (logand most-positive-fixnum (1+ id))))
    (defun get-next-id (id)
      (declare (type fixnum id))
      (the fixnum
        (if (= id most-positive-fixnum)
            0
            (1+ id))))))

(defmacro next-id (place)
  `(setf ,place (get-next-id ,place)))


;;;; ** Implementation classes

(defstruct tlog
  "A transaction log (TLOG) is a record of the reads and writes
to transactional memory performed during a transaction.

Transaction logs are automatically populated by reading and writing
transactional objects (TOBJs) or transactional variables (TVARs),
and are later committed to memory if the transaction completes successfully."

  ;; TVARs read during transaction, mapped to their read value
  (reads (make-hash-table :test 'eq) :type hash-table) ;; tlog-reads
  ;; TVARs written during transaction, mapped to their new values
  (writes (make-hash-table :test 'eq) :type hash-table) ;; tlog-writes

  ;; Parent of this TLOG. Used by ORELSE for nested transactions
  (parent    nil :type (or null tlog)) ;; tlog-parent
  (lock      nil) ;; tlog-lock
  (semaphore nil) ;; tlog-semaphore

  ;; Flag to prevent TLOGs from sleeping. Set by TVARs when they change.
  (prevent-sleep nil :type boolean) ;; tlog-prevent-sleep
  ;; Functions to call immediately before committing TLOG.
  (before-commit nil :type (or null vector)) ;; tlog-before-commit
  ;; Functions to call immediately after committing TLOG.
  (after-commit  nil :type (or null vector)) ;; tlog-after-commit

  (id (next-id *tlog-id*) :type fixnum :read-only t)) ;; tlog-id


(defmethod id-of ((log tlog))
  (tlog-id log))



(declaim (type symbol +unbound+))
(defvar +unbound+ (gensym "UNBOUND-"))


(defstruct tvar
  "A transactional variable (TVAR) is the smallest unit
of transactional memory.
It contains a single value that can be read or written during a transaction
using ($ tvar) and (SETF ($ tvar) value).

TVARs are seldom used directly, since transactional objects (TOBJs) wrap them
with a more intuitive and powerful interface: you can read and write normally
the slots of a transactional object (with slot-value, accessors ...),
and behind the scenes the slots will be stored in transactional memory implemented by TVARs."

  (value +unbound+)                             ;; tvar-value
  (lock (make-lock "TVAR") :read-only t)        ;; tvar-lock
  (waiting-for nil :type (or null hash-table))  ;; tvar-waiting-for
  (waiting-lock (make-lock "WAITING-FOR-LOCK") :read-only t) ;; tvar-waiting-lock
  (id (next-id *tvar-id*) :type fixnum :read-only t))       ;; tvar-id

(defmethod id-of ((var tvar))
  (tvar-id var))

(declaim (inline raw-value-of (setf raw-value-of)))
(defun raw-value-of (var)
  (declare (type tvar var))
  (tvar-value var))

(defun (setf raw-value-of) (value var)
  (declare (type tvar var))
  (setf (tvar-value var) value))





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


(defun ensure-thread-initial-bindings (&rest syms-and-forms)
  (declare (type list syms-and-forms))
  (loop for sym-and-form in syms-and-forms do
       (unless (assoc (first sym-and-form) bt:*default-special-bindings* :test 'eq)
         (push sym-and-form bt:*default-special-bindings*))))

(defmacro save-thread-initial-bindings (&rest syms)
  `(ensure-thread-initial-bindings
    ,@(loop for sym in syms collect `(cons ',sym ,sym))))

(eval-when (:load-toplevel :execute)
  (save-thread-initial-bindings *tlog* *record-to-tlogs* *hide-tvars*))
