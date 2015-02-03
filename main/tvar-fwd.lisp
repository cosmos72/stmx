;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013-2014 Massimiliano Ghilardi
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

;;;; ** constants

(declaim (type symbol +unbound-tvar+))
;; do NOT use (define-constant ... (if (boundp '...) (symbol-value '...) <actual-definition>))
;; as it causes bugs at least on SBCL
(define-global +unbound-tvar+ (gensym (symbol-name 'unbound-tvar-)))

(declaim (type version-type +invalid-version+))
(defconstant +invalid-version+ (- +global-clock-delta+))


;;;; ** tvar approximate counter (fast but not fully exact in multi-threaded usage)

(declaim (type fixnum +tvar-id+))
(define-global +tvar-id+ -1)

(declaim (inline get-next-id))
(defun get-next-id (id)
  (declare (type fixnum id))
  (the fixnum
    #?+fixnum-is-powerof2
    (logand most-positive-fixnum (1+ id))

    #?-fixnum-is-powerof2
    (if (= id most-positive-fixnum)
        0
        (1+ id))))

(defmacro tvar-id/next ()
  `(setf +tvar-id+ (get-next-id +tvar-id+)))





;;;; ** implementation class: TVAR, a versioned "box" containing a value and a lock.
;;;; ** Used as base class for TVAR.

(declaim (inline make-tvar))

(defstruct (tvar #?+(eql tvar-lock :mutex) (:include mutex))
  "a transactional variable (tvar) is the smallest unit of transactional memory.
it contains a single value that can be read or written during a transaction
using ($-slot var) and (setf ($-slot var) value).

tvars are seldom used directly, since transactional objects (tobjs) wrap them
with a more convenient interface: you can read and write normally the slots
of a transactional object (with slot-value, accessors ...), and behind
the scenes the slots will be stored in transactional memory implemented by tvars."

  (version +invalid-version+ :type version-type)
  (value   +unbound-tvar+    :type t)

  (id +invalid-version+ :type fixnum :read-only t)       ;; tvar-id for debugging purposes

  (waiting-for nil :type (or null hash-table))           ;; tvar-waiting-for
  (waiting-lock (make-lock "TVAR-WAITING") :read-only t));; tvar-waiting-lock



;;;; ** Reading and writing


(defmethod id-of ((var tvar))
  (the fixnum (tvar-id var)))


(declaim (ftype (function (#-ecl tvar #+ecl t) t) raw-value-of)
         (inline raw-value-of))

(defun raw-value-of (var)
  "return the current value of VAR, or +unbound-tvar+ if VAR is not bound to a value.
This finction intentionally ignores transactions and it is only useful
for debugging purposes. please use ($-slot var) instead."
  (declare (type tvar var))
  (tvar-value var))






(declaim (ftype (function (tvar) (values atomic-counter-num t)) %tvar-version-and-value)
         (inline
           %tvar-version-and-value))

(defun %tvar-version-and-value (var)
  "Internal function used only by TVAR-VALUE-AND-VERSION-OR-FAIL."
  (declare (type tvar var))
  
  ;; if we have memory barriers, we MUST use them
  ;; and read version first, then value
  ;; otherwise we VIOLATE read consistency!
  ;; see doc/consistent-reads.md for the gory details
  #?+mem-rw-barriers
  (values (progn (mem-read-barrier) (tvar-version var))
          (progn (mem-read-barrier) (tvar-value var)))
        
  #?-mem-rw-barriers
  ;; no way to guarantee the order
  (values (tvar-version var) (tvar-value var)))
   





(declaim (ftype (function (tvar) (values t atomic-counter-num bit))
		tvar-value-and-version-or-fail)
         (inline
           tvar-value-and-version-or-fail))

(defun tvar-value-and-version-or-fail (var)
  "return as multiple values:
1) the current value of VAR, or +unbound-tvar+ if VAR is not bound to a value.
2) the current version of VAR, excluding any lock bit.
3) 0 if VAR is if free, or 1 if it's locked or could not be read reliably
   - for example because the version changed while reading."
  (declare (type tvar var))

  #?+(eql tvar-lock :single-thread)
  ;; no concurrent modifications to protect against
  (multiple-value-bind (version value) (%tvar-version-and-value var)
    (values value version 0))
  
  
  #?+(eql tvar-lock :bit)
  ;; lock is lowest bit of tvar-version. extract and check it.
  ;; we must get version, value and version again EXACTLY in this order.
  ;; see doc/consistent-reads.md for the gory details
  (multiple-value-bind (version0+lock value) (%tvar-version-and-value var)
    (let1 version1+lock (progn (mem-read-barrier) (tvar-version var))
      
      (declare (type atomic-counter-num version0+lock version1+lock))

      (let* ((version1 (logand version1+lock (lognot 1)))
             (lock1    (logand version1+lock 1))
             (fail     (logior lock1
                               (if (= version0+lock version1+lock) 0 1))))
        (values value version1 fail))))


  #?+(eql tvar-lock :mutex)
  (progn
    #?+(and mutex-owner mem-rw-barriers (not (eql mem-rw-barriers :trivial)))
    ;; this case is tricky. we can get away with unlocked reads
    ;; by reading in order: TVAR version, value, lock status, version again.
    ;; again, see doc/consistent-reads.md for the gory details
    (multiple-value-bind (version0 value) (%tvar-version-and-value var)
      (let ((free? (mutex-is-free? (the mutex var)))
            (version1 (tvar-version var)))
        
        (declare (type boolean free?)
                 (type atomic-counter-num version0 version1))

        (let1 fail (if (and free? (= version0 version1)) 0 1)

          (values value version1 fail))))

    #?+(and mutex-owner mem-rw-barriers (eql mem-rw-barriers :trivial))
    ;; trivial memory barriers are even more tricky. we cannot guarantee
    ;; the order while reading TVAR value and version, so we must read both twice,
    ;; see doc/consistent-reads.md for the gory details
    (multiple-value-bind (version0 value0) (%tvar-version-and-value var)
      (let1 free? (mutex-is-free? (the mutex var))
        (multiple-value-bind (version1 value1) (%tvar-version-and-value var)

          (declare (type boolean free?)
                   (type atomic-counter-num version0 version1))

          (let1 fail (if (and free?
                              (eq value0 value1)
                              (= version0 version1))
                         0 1)

            (values value1 version1 fail)))))

    #?-(and mutex-owner mem-rw-barriers)
    (let ((acquired (try-acquire-mutex/catch-recursion (the mutex var))))
      (if acquired ;; possible values are t :recursion nil
          (multiple-value-bind (version value) (%tvar-version-and-value var)
            (when (eq acquired t)
              (release-mutex (the mutex var)))
            (values value version 0))

          (values nil +invalid-version+ 1)))))



(declaim (ftype (function (tvar t atomic-counter-num) (values t))
		set-tvar-value-and-version)
         (inline
           set-tvar-value-and-version))

(defun set-tvar-value-and-version (var value version)
  "Set the VALUE and VERSION of VAR. If lock is a bit inside TVAR version,
also set it (which may unlock VAR!). Return VALUE."
  (declare (type tvar var)
           (type atomic-counter-num version))

  (progn
    ;; if we have memory barriers, we MUST use them
    ;; and write value first, then version
    ;; otherwise we VIOLATE read consistency!
    #?+mem-rw-barriers
    (progn
      (mem-write-barrier) (setf (tvar-value   var) value)
      (mem-write-barrier) (setf (tvar-version var) version)
      value)

    #?-mem-rw-barriers
    ;; no memory barriers. easy here, but guaranteeing read consistency
    ;; will be painful
    (setf (tvar-version var) version
          (tvar-value   var) value)))





;;;; ** Locking and unlocking

(declaim (ftype (function (tvar)        boolean) try-lock-tvar)
         (ftype (function (tvar)        null)    unlock-tvar)
         (inline
           try-lock-tvar unlock-tvar))

(defun try-lock-tvar (var)
  "Try to lock VAR. Return T if locked successfully, otherwise return NIL."
  (declare (ignorable var))

  #?+(eql tvar-lock :single-thread)
  ;; no lock to take
  t
  
  #?+(eql tvar-lock :bit)
  (let1 version (progn (mem-read-barrier) (the version-type (tvar-version var)))
    (declare (type atomic-counter-num version))
    (when (zerop (logand version 1))
      (let* ((new-version (the version-type (logior version 1)))
             (old-version (atomic-compare-and-swap (tvar-version var)
                                                   version new-version)))
        (= version old-version))))

  #?+(eql tvar-lock :mutex)
  (try-acquire-mutex (the mutex var)))
  
  
(defun unlock-tvar (var)
  "Unlock VAR. always return NIL."
  (declare (ignorable var))

  #?+(eql tvar-lock :single-thread)
  ;; no lock to release
  nil

  #?+(eql tvar-lock :bit)
  (let1 version-unlocked (logand (the version-type (tvar-version var)) (lognot 1))
    (mem-write-barrier)
    (setf (tvar-version var) version-unlocked)
    nil)

  #?+(eql tvar-lock :mutex)
  (release-mutex (the mutex var)))
