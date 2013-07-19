;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013 massimiliano ghilardi
;;
;; this library is free software: you can redistribute it and/or
;; modify it under the terms of the lisp lesser general public license
;; (http://opensource.franz.com/preamble.html), known as the llgpl.
;;
;; this library is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.
;; see the lisp lesser general public license for more details.


(in-package :stmx)

(enable-#?-syntax)


;;;; * support classes for TXHASH-TABLE

(declaim (inline txpair))

(defstruct (txpair (:constructor txpair))
  (key   nil :type tvar)
  (value nil :type t)
  (next  nil :type (or null txpair))
  (rest  nil :type (or null txpair))) ;; used by txfifo for intrusive list


(defstruct (txfifo (:constructor %make-txfifo))
  (front nil :type (or null txpair))
  (back  nil :type (or null txpair)))


(declaim (inline make-txfifo))
(defun make-txfifo ()
  "Create and return a new, empty TXFIFO."
  (the txfifo (%make-txfifo)))








;;;; ** TXHASH-TABLE, a hash table specialized for TVAR keys

(defconstant +txhash-default-capacity+ 4)

(defstruct (txhash-table (:constructor %make-txhash-table))
  "a hash table specialized for tvar keys."
  (vec   0   :type simple-vector)
  (count 0   :type fixnum)
  (pool  nil :type t))

(declaim (ftype (function (&key (:initial-capacity fixnum))
                          txhash-table) make-txhash-table))

(defun make-txhash-table (&key (initial-capacity +txhash-default-capacity+))
  (declare (type fixnum initial-capacity))

  (unless (zerop (logand initial-capacity (1- initial-capacity)))
    (error "~A invalid initial capacity ~A: expecting a power of two."
           'txhash-table initial-capacity))

  (the txhash-table
    (%make-txhash-table :vec (make-array initial-capacity :initial-element nil)
                        :count 0)))










;;;; ** implementation class: tlog

(deftype tlog-func-vector () '(and vector (not simple-array)))

(defstruct tlog
  "a transaction log (tlog) is a record of the reads and writes
to transactional memory performed during a transaction.

transaction logs are automatically populated by reading and writing
transactional objects (tobjs) or transactional variables (tvars),
and are later committed to memory if the transaction completes successfully."

  ;; tlog-reads: tvars read during transaction, mapped to their read value
  (reads (make-txhash-table)      :type txhash-table)

  ;; tlog-writes: tvars written during transaction, mapped to their new values
  (writes (make-txhash-table)     :type txhash-table)

  (locked (make-txfifo)           :type txfifo :read-only t)

  (read-version +invalid-version+ :type version-type) ;; tlog-read-version

  ;; Parent of this TLOG. Used by ORELSE for nested transactions
  (parent    nil :type (or null tlog)) ;; tlog-parent
  (lock      nil) ;; tlog-lock
  (semaphore nil) ;; tlog-semaphore

  ;; Flag to prevent TLOGs from sleeping. Set by TVARs when they change.
  (prevent-sleep nil       :type boolean) ;; tlog-prevent-sleep
  ;; Functions to call immediately before committing TLOG.
  (before-commit nil      :type (or null tlog-func-vector)) ;; tlog-before-commit
  ;; Functions to call immediately after committing TLOG.
  (after-commit  nil      :type (or null tlog-func-vector))) ;; tlog-after-commit



(defmethod id-of ((log tlog))
  (tlog-read-version log))












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


(declaim (inline transaction?))
(defun transaction? ()
  "Return true if inside a software or hardware transaction."
  (or (hw-transaction-supported-and-running?)
      *record-to-tlogs*))




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



;;;; ** Current software transaction log

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


;;;; ** Current hardware transaction log

(declaim (type version-type *hw-tlog-write-version*))
(defvar *hw-tlog-write-version* +invalid-version+)

(defmacro hw-tlog-write-version ()
  "Return the WRITE-VERSION for the current hardware transaction"
  '*hw-tlog-write-version*)



(eval-when (:load-toplevel :execute)
  (save-thread-initial-bindings *tlog* *record-to-tlogs* *hide-tvars* *hw-tlog-write-version*))



;;;; ** Retrying

(defmacro maybe-yield-before-rerun ()
  #+never nil
  #-always (thread-yield))


(define-condition stmx.control-error (control-error)
  ()
  (:documentation "Parent class of all STMX errors"))


(define-condition retry-error (stmx.control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'retry 'atomic)))

  (:documentation "Signalled by RETRY. It is captured by ATOMIC and ORELSE,
so it is visible to application code only in case of bugs"))



(define-condition rerun-error (stmx.control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Transactional memory conflict detected inside ~A.
If you see this error, there is a bug either in STMX or in application code.
Possible reason: code registered with ~A attempted to read transactional memory
not already read or written during the transaction" 'orelse 'after-commit)))

  (:documentation "Signalled by ORELSE to force re-running the parent transaction
in case of conflicts. It is captured by ATOMIC, so it is visible
to application code only in case of bugs"))



(let1 retry-error-obj (make-condition 'retry-error)
  (defun retry ()
    "Abort the current transaction and re-run it again from the beginning.

Before re-executing, the transaction will wait on all variables that it read
until at least one of them changes."
    (error retry-error-obj)))


(let1 rerun-error-obj (make-condition 'rerun-error)
  (defun rerun ()
    "Abort the current transaction and immediately re-run it from the
beginning without waiting. Used by ORELSE."
    (error rerun-error-obj)))

  


