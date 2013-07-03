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

;;;; ** constants

(defconstant +tlog-counter-delta+ 2
  "*tlog-counter* is incremented by 2 each time: the lowest bit is used
as \"locked\" flag in TVARs versioning.")

(declaim (type symbol +unbound-tvar+))
(defvar +unbound-tvar+ (gensym (symbol-name 'unbound-tvar-)))

(declaim (type fixnum +invalid-version+))
(defconstant +invalid-version+ (- +tlog-counter-delta+))

(declaim (type cons +versioned-unbound-tvar+))
(defvar +versioned-unbound-tvar+ (cons +invalid-version+ +unbound-tvar+))


;;;; ** implementation class: TVAR, a versioned "box" containing a value and a lock.
;;;; ** Used as base class for TVAR.


(defstruct (tvar #?-fast-lock (:include mutex))
  "a transactional variable (tvar) is the smallest unit of transactional memory.
it contains a single value that can be read or written during a transaction
using ($ var) and (setf ($ var) value).

tvars are seldom used directly, since transactional objects (tobjs) wrap them
with a more convenient interface: you can read and write normally the slots
of a transactional object (with slot-value, accessors ...), and behind
the scenes the slots will be stored in transactional memory implemented by tvars."

  (version +invalid-version+ :type fixnum)
  (value   +unbound-tvar+    :type t)

  (id +invalid-version+ :type fixnum :read-only t)       ;; tvar-id

  (waiting-for nil :type (or null hash-table))           ;; tvar-waiting-for
  (waiting-lock (make-lock "tvar-waiting") :read-only t));; tvar-waiting-lock



;;;; ** Reading and writing


(defmethod id-of ((var tvar))
  (the fixnum (tvar-id var)))


(declaim (ftype (function (#-ecl tvar #+ecl t) t) raw-value-of)
         (inline raw-value-of))

(defun raw-value-of (var)
  "return the current value of VAR, or +unbound-tvar+ if VAR is not bound to a value.
This finction intentionally ignores transactions and it is only useful
for debugging purposes. please use ($ var) instead."
  (declare (type tvar var))
  (tvar-value var))






(declaim (ftype (function (tvar) (values fixnum t)) %tvar-version-and-value)
         (inline
           %tvar-version-and-value))

(defun %tvar-version-and-value (var)
  "Internal function used only by TVAR-VALUE-VERSION-AND-LOCK."
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
   





(declaim (ftype (function (tvar) (values t fixnum bit)) tvar-value-and-version-or-fail)
         (inline
           tvar-value-and-version-or-fail))

(defun tvar-value-and-version-or-fail (var)
  "return as multiple values:
1) the current value of VAR, or +unbound-tvar+ if VAR is not bound to a value.
2) the current version of VAR, excluding any lock bit.
3) 0 if VAR is if free, or 1 if it's locked or could not be read reliably
   - for example because the version changed while reading."
  (declare (type tvar var))

  #?+fast-lock
  ;; lock is lowest bit of tvar-version. extract and check it.
  ;; we must get version, value and version again EXACTLY in this order.
  ;; see doc/consistent-reads.md for the gory details
  (multiple-value-bind (version0+lock value) (%tvar-version-and-value var)
    (let1 version1+lock (progn (mem-read-barrier) (tvar-version var))
      
      (declare (type fixnum version0+lock version1+lock))

      (let* ((version1 (logand version1+lock (lognot 1)))
             (lock1    (logand version1+lock 1))
             (fail     (logior lock1
                               (if (= version0+lock version1+lock) 0 1))))
        (values value version1 fail))))

  #?-fast-lock
  (progn
    #?+(and mutex-owner mem-rw-barriers (not (eql mem-rw-barriers :trivial)))
    ;; this case is tricky. we can get away with unlocked reads
    ;; by reading in order: TVAR version, value, lock status, version again.
    ;; again, see doc/consistent-reads.md for the gory details
    (multiple-value-bind (version0 value) (%tvar-version-and-value var)
      (let ((free? (mutex-is-free? (the mutex var)))
            (version1 (tvar-version var)))
        
        (declare (type boolean free?)
                 (type fixnum version0 version1))

        (let1 fail (if (and free? (= version0 version1)) 0 1)

          (values value version1 fail))))

    #?+(and mutex-owner mem-rw-barriers (eql mem-rw-barriers :trivial))
    ;; trivial memory barriers are even more tricky. we cannot guarantee
    ;; the order while reading TVAR value and version, so we must read both twice
    ;; as usual, see doc/consistent-reads.md for the gory details
    (multiple-value-bind (version0 value0) (%tvar-version-and-value var)
      (let1 free? (mutex-is-free? (the mutex var))
        (multiple-value-bind (version1 value1) (%tvar-version-and-value var)

          (declare (type boolean free?)
                   (type fixnum version0 version1))

          (let1 fail (if (and free?
                              (eq value0 value1)
                              (= version0 version1))
                         0 1)

            (values value1 version1 fail)))))

    #?-(and mutex-owner mem-rw-barriers)
    ;; no mutex owner, or no memory barriers - not even trivial ones.
    ;; resort to locking TVAR... Horrible and slow
    (if (try-acquire-mutex (the mutex var))

        (multiple-value-bind (version value) (%tvar-version-and-value var)
          (release-mutex (the mutex var))
          (values value version 0))

        (values nil +invalid-version+ 1))))



(declaim (ftype (function (tvar t fixnum) (values t)) set-tvar-value-and-version)
         (inline
           set-tvar-value-and-version))

(defun set-tvar-value-and-version (var value version)
  "Set the VALUE and VERSION of VAR. If lock is a bit inside TVAR version,
also set it (which may unlock VAR!). Return VALUE."
  (declare (type tvar var)
           (type fixnum version))

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
  #?+fast-lock
  (let1 version (tvar-version var) 
    (declare (type fixnum version))
    (when (zerop (logand version 1))
      (let1 old-version (atomic-compare-and-swap (tvar-version var)
                                                 version (logior version 1))
        (= version old-version))))

  #?-fast-lock
  (try-acquire-mutex (the mutex var)))
  
  
(defun unlock-tvar (var)
  "Unlock VAR. always return NIL."
  #?+fast-lock
  (let* ((version+lock     (the fixnum (tvar-version var)))
         (version-unlocked (logand version+lock (lognot 1))))
    (mem-write-barrier)
    (setf (tvar-version var) version-unlocked)
    nil)
  #?-fast-lock
  (release-mutex (the mutex var)))
