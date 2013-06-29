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

  #?+unwrapped-tvar
  (version +invalid-version+ :type fixnum)
  #?+unwrapped-tvar
  (value   +unbound-tvar+    :type t)

  #?-unwrapped-tvar
  (versioned-value +versioned-unbound-tvar+)             ;; tvar-versioned-value

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
  #?+unwrapped-tvar
  (tvar-value var)
  #?-unwrapped-tvar
  (rest (tvar-versioned-value var)))






(declaim (ftype (function (tvar) (values t fixnum))     tvar-value-and-version)
         (inline
           tvar-value-and-version))

(defun tvar-value-and-version (var)
  "Internal function used only by TVAR-VALUE-VERSION-AND-LOCK."
  (declare (type tvar var))
  
  #?+unwrapped-tvar
  (progn
    ;; if we have memory barriers, we MUST use them
    ;; and read value first, then version
    ;; otherwise we VIOLATE read consistency!
    #?+mem-rw-barriers
    (values (progn (mem-read-barrier) (tvar-value var))
            (progn (mem-read-barrier) (tvar-version var)))
        
    #?-mem-rw-barriers
    ;; no way to guarantee the order
    (values (tvar-value var) (tvar-version var)))
    
  #?-unwrapped-tvar
  (let* ((pair    (tvar-versioned-value var))
         (version (first pair))
         (value   (rest pair)))
    (values value version)))





(declaim (ftype (function (tvar t fixnum) (values t)) set-tvar-value-and-version)
         (inline
           set-tvar-value-and-version))

(defun set-tvar-value-and-version (var value version)
  "Set the VALUE and VERSION of VAR. If lock is a bit inside TVAR version,
also set it (which may unlock VAR!). Return VALUE."
  (declare (type tvar var)
           (type fixnum version))

  #?+unwrapped-tvar
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
          (tvar-value   var) value))
    
  #?-unwrapped-tvar
  (progn
    (setf (tvar-versioned-value var) (cons version value))
    value))




(declaim (ftype (function (tvar) (values t fixnum bit)) tvar-value-version-and-lock)
         (inline
           tvar-value-version-and-lock))

(defun tvar-value-version-and-lock (var)
  "return as multiple values:
1) the current value of VAR, or +unbound-tvar+ if VAR is not bound to a value.
2) the current version of VAR, excluding the lock bit.
3) 0 if VAR is if free, or 1 if it's locked."
  (declare (type tvar var))

  #?+fast-lock
  ;; lock is lowest bit of tvar-version
  (multiple-value-bind (value version+lock) (tvar-value-and-version var)
    (declare (type fixnum version+lock))

    (let ((version (logand version+lock (lognot 1)))
          (lock    (logand version+lock 1)))
    
      (values value version lock)))

  #?-fast-lock
  (progn
    #?+(and mem-rw-barriers mutex-owner)
    ;; this case is tricky. we can get away with unlocked reads
    ;; by checking for lock owner BEFORE reading tvar and still guarantee
    ;; read consistency in every transaction. Demonstration:
    ;;
    ;; if A and B are TVARs being committed by a transaction TX1,
    ;; even if another transaction TX2 has the same TX counter,
    ;; in order to get the old value of one TVAR (say B)
    ;; and the new value of the other (say A), i.e. to violate read consistency,
    ;; TX2 will check for B lock owner BEFORE reading B
    ;; but after getting the TX counter.
    ;;
    ;; Then, if B is found locked -> the old value may have been read, but lock on B
    ;;                               causes a (rerun) exactly to avoid inconsistencies
    ;; if B is found unlocked -> the NEW value and version will be already in B
    ;;                           (thus read by TX2 since memory barriers are used or no-op)
    ;;                           not the OLD one, so no inconsistencies: in the worst
    ;;                           case, a too-high version will trigger a (rerun)
    ;;
    ;; So read lock first, then value and version
    (let1 free? (mutex-is-free? (the mutex var))
      (multiple-value-bind (value version) (tvar-value-and-version var)
        (values value version (if free? 0 1))))

    #?-(and mem-rw-barriers mutex-owner)
    ;; general case: acquire lock around EVERY read of a TVAR.
    ;; works, but it's horribly slow.
    ;; Without memory barriers (even fake ones) or mutex-owner,
    ;; we cannot do better :(
    (if (try-acquire-mutex (the mutex var))

        (multiple-value-bind (value version) (tvar-value-and-version var)
          (release-mutex (the mutex var))
          (values value version 0))

        (values nil +invalid-version+ 1))))





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
