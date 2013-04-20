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
   (fast-lock-status t :type t))


 (declaim (ftype (function () fast-lock) make-fast-lock)
          (inline make-fast-lock))

 (defun make-fast-lock ()
   "Create and return a FAST-LOCK."
   (the fast-lock (%make-fast-lock)))
    


 (declaim (ftype (function (fast-lock) boolean)
                 try-acquire-fast-lock
                 release-fast-lock)
          (inline try-acquire-fast-lock
                  release-fast-lock))

 (defun try-acquire-fast-lock (fast-lock)
   "Try to acquire FAST-LOCK. Return T if successful,
or NIL if FAST-LOCK was already locked."
   (sb-ext:compare-and-swap (fast-lock-fast-lock-status fast-lock) t nil))

 (defun release-fast-lock (fast-lock)
   "Release FAST-LOCK and return T,
Does not signal any error if FAST-LOCK was already unlocked."
   (sb-thread:barrier (:write))
   (setf (fast-lock-fast-lock-status fast-lock) t)))
  

