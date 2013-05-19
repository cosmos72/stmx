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



;;;; ** nowait mutex - uses fast atomic ops if available,
;;;;    otherwise falls back on bordeaux-threads locks



(defstruct (mutex (:constructor %make-mutex) (:conc-name))
  #+stmx.have-atomic-ops
  (mutex-owner nil :type atomic-t)
  #-stmx.have-atomic-ops
  (mutex-lock (make-lock "MUTEX") :read-only t))





(declaim (ftype (function () mutex) make-mutex)
         (inline make-mutex))

(defun make-mutex ()
  "Create and return a MUTEX."
  (the mutex (%make-mutex)))
    




(declaim (ftype (function (mutex) boolean)  try-acquire-mutex)
         (ftype (function (mutex) null)     release-mutex)
          (inline
            try-acquire-mutex release-mutex))

(defun try-acquire-mutex (mutex)
  "Try to acquire MUTEX. Return T if successful,
or NIL if MUTEX was already locked."
  (declare (type mutex mutex))
  #+stmx.have-atomic-ops
  (atomic-write-barrier
    (null
     (sb-ext:compare-and-swap (mutex-owner mutex) nil *current-thread*)))
  #-stmx.have-atomic-ops
  (bt:acquire-lock (mutex-lock mutex) nil))

   
(defun release-mutex (mutex)
  "Release MUTEX. Return NIL. Consequences are undefined if MUTEX
is locked by another thread or is already unlocked."
  #+stmx.have-atomic-ops
  (progn
    (atomic-write-barrier)
    (setf (mutex-owner mutex) nil))
  #-stmx.have-atomic-ops
  (bt:release-lock (mutex-lock mutex)))





#+stmx.have-atomic-ops
(eval-always
  (declaim (ftype (function (mutex) boolean) mutex-is-free? mutex-is-own-or-free?)
	   (inline
	     mutex-is-free? mutex-is-own-or-free?))


  (defun mutex-is-free? (mutex)
    "Return T if MUTEX is free. Return NIL if MUTEX is currently locked
by this thread or some other thread."
    (atomic-read-barrier)
    (let ((owner
	   (atomic-read-barrier
	     (mutex-owner mutex))))
      (eq owner nil)))


  (defun mutex-is-own-or-free? (mutex)
    "Return T if MUTEX is free or locked by current thread.
Return NIL if MUTEX is currently locked by some other thread."
    (atomic-read-barrier)
    (let ((owner
	   (atomic-read-barrier
	     (mutex-owner mutex))))
      (or
       (eq owner nil)
       (eq owner *current-thread*)))))



