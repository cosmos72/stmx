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

;;;; ** TLOG global versioning - exact, atomic counter

(declaim (type atomic-counter *tlog-counter*))
(defvar *tlog-counter* (make-atomic-counter))

(declaim (type fixnum +invalid-counter+))
(defconstant +invalid-counter+ -1)

;;;; ** Implementation class: TLOG

(deftype tlog-func-vector () '(and vector (not simple-array)))

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
  (before-commit nil :type (or null tlog-func-vector)) ;; tlog-before-commit
  ;; Functions to call immediately after committing TLOG.
  (after-commit  nil :type (or null tlog-func-vector)) ;; tlog-after-commit

  (id +invalid-counter+ :type fixnum)) ;; tlog-id


(defmethod id-of ((log tlog))
  (tlog-id log))





;;;; ** TVAR approximate counter (fast but not fully exact in multi-threaded usage)

(declaim (type fixnum *tvar-id*))
(defvar *tvar-id* 0)

(declaim (inline get-next-id))
(defun get-next-id (id)
  (declare (type fixnum id))
  (the fixnum
    #+stmx-fixnum-is-power-of-two
    (logand most-positive-fixnum (1+ id))

    #-stmx-fixnum-is-power-of-two
    (if (= id most-positive-fixnum)
        0
        (1+ id))))

(defmacro next-id (place)
  `(setf ,place (get-next-id ,place)))


;;;; ** Implementation class: TVAR


(declaim (type symbol +unbound+))
(defvar +unbound+ (gensym "UNBOUND-"))

(declaim (type cons +versioned-unbound+))
(defvar +versioned-unbound+ (cons +invalid-counter+ +unbound+))


(defstruct (tvar #+stmx-have-fast-lock (:include fast-lock))

  "A transactional variable (TVAR) is the smallest unit of transactional memory.
It contains a single value that can be read or written during a transaction
using ($ var) and (SETF ($ var) value).

TVARs are seldom used directly, since transactional objects (TOBJs) wrap them
with a more convenient interface: you can read and write normally the slots
of a transactional object (with slot-value, accessors ...), and behind
the scenes the slots will be stored in transactional memory implemented by TVARs."

  (versioned-value +versioned-unbound+)         ;; tvar-versioned-value

  #-stmx-have-fast-lock
  (lock (make-lock "TVAR"))

  (waiting-for nil :type (or null hash-table))           ;; tvar-waiting-for
  (waiting-lock (make-lock "TVAR-WAITING") :read-only t) ;; tvar-waiting-lock
  (id (next-id *tvar-id*) :type fixnum :read-only t))    ;; tvar-id


(defmethod id-of ((var tvar))
  (tvar-id var))


(declaim (ftype (function (tvar) fixnum) tvar-version)
         (inline tvar-version))
(defun tvar-version (var)
  (first (tvar-versioned-value var)))


(defun raw-value-of (var)
  "Return the current value of VAR, or +unbound+ if VAR is not bound to a value.
This method intentionally ignores transactions and it is only useful for debugging
purposes. Please use ($ var) instead."
  (declare (type tvar var))
  (rest (tvar-versioned-value var)))


(defun (setf raw-value-of) (value var)
  "Bind VAR to VALUE and return VALUE. This method intentionally ignores
transactions and it is only useful for debugging purposes.
Please use (setf ($ var) value) instead."
  (declare (type tvar var))
  (setf (tvar-versioned-value var) (cons (incf-atomic-counter *tlog-counter*) value))
  value)





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


(defmacro set-tlog-version (log)
  `(setf (tlog-id ,log) (get-atomic-counter *tlog-counter*)))


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





;;;; ** Retrying

(define-condition stmx-control-error (control-error)
  ()
  (:documentation "Parent class of all STMX errors"))


(define-condition retry-error (stmx-control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to ~A outside an ~A block" 'retry 'atomic)))

  (:documentation "Signalled by RETRY. It is captured by ATOMIC and ORELSE,
so it is visible to application code only in case of bugs"))



(define-condition rerun-error (stmx-control-error)
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

  


