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

 (defstruct (fast-lock (:constructor %make-fast-lock))
   (fast-lock-owner nil :type t))


 (declaim (ftype (function () fast-lock) make-fast-lock)
          (inline make-fast-lock))

 (defun make-fast-lock ()
   "Create and return a FAST-LOCK."
   (the fast-lock (%make-fast-lock)))
    


 (declaim (ftype (function (fast-lock) boolean)   try-acquire-fast-lock)
          (ftype (function (fast-lock) null)      release-fast-lock)
          (ftype (function (fast-lock) (values (or null thread) &optional)) fast-lock-owner)
          (inline
            try-acquire-fast-lock release-fast-lock fast-lock-owner))

 (defun try-acquire-fast-lock (fast-lock)
   "Try to acquire FAST-LOCK. Return T if successful,
or NIL if FAST-LOCK was already locked."
   #+stmx-have-sbcl.atomic-ops
   (eq nil
       (sb-thread:barrier (:write)
         (sb-ext:compare-and-swap (fast-lock-fast-lock-owner fast-lock) nil (bt:current-thread)))))

 (defun release-fast-lock (fast-lock)
   "Release FAST-LOCK. Return NIL.
Does not signal any error if FAST-LOCK was already unlocked."
   #+stmx-have-sbcl.atomic-ops
   (progn
     (sb-thread:barrier (:write))
     (setf (fast-lock-fast-lock-owner fast-lock) nil)))

 (defun fast-lock-owner (fast-lock)
   "Return the thread that acquired FAST-LOCK, or NIL if FAST-LOCK is free."
   (sb-thread:barrier (:read))
   (fast-lock-fast-lock-owner fast-lock)))
   

