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


(in-package :stmx.lang)

;;;; ** fast nowait lock

#+stmx-have-fast-lock
(eval-always

 (defstruct (fast-lock (:constructor %make-fast-lock) (:conc-name))
   (fast-lock-owner nil :type t))

 (declaim (ftype (function () fast-lock) make-fast-lock)
          (inline make-fast-lock))

 (defun make-fast-lock ()
   "Create and return a FAST-LOCK."
   (the fast-lock (%make-fast-lock)))
    


 (declaim (ftype (function (fast-lock) boolean)  try-acquire-fast-lock)
          (ftype (function (fast-lock) null)     release-fast-lock)
          (ftype (function (fast-lock) boolean)  fast-lock-is-own-or-free?)
          (inline
            try-acquire-fast-lock release-fast-lock fast-lock-is-own-or-free?))

 (defun try-acquire-fast-lock (fast-lock)
   "Try to acquire FAST-LOCK. Return T if successful,
or NIL if FAST-LOCK was already locked."
   (declare (type fast-lock fast-lock))
   #+stmx-have-sbcl.atomic-ops
   (sb-thread:barrier (:write)
     (null
      (sb-ext:compare-and-swap (fast-lock-owner fast-lock) nil *current-thread*)))
   #-stmx-have-sbcl.atomic-ops
   (error "No available implementation for TRY-ACQUIRE-FAST-LOCK"))

 (defun release-fast-lock (fast-lock)
   "Release FAST-LOCK. Return NIL.
Does not signal any error if FAST-LOCK was already unlocked."
   #+stmx-have-sbcl.atomic-ops
   (progn
     (sb-thread:barrier (:write))
     (setf (fast-lock-owner fast-lock) nil))
   #-stmx-have-sbcl.atomic-ops
   (error "No available implementation for RELEASE-FAST-LOCK"))

 (defun fast-lock-is-own-or-free? (fast-lock)
   "Return T if FAST-LOCK is free or locked by current thread.
Return NIL if FAST-LOCK is currently locked by some other thread."
   #+stmx-have-sbcl.atomic-ops
   (progn
     (sb-thread:barrier (:read))
     (let ((owner
            (sb-thread:barrier (:read)
              (fast-lock-owner fast-lock))))
       (or
        (eq owner nil)
        (eq owner *current-thread*))))
   #-stmx-have-sbcl.atomic-ops
   (error "No available implementation for FAST-LOCK-IS-OWN-OR-FREE?")))
       
   
