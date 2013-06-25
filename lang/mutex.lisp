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

(enable-#?-syntax)

;;;; ** Faster replacement for bordeaux-threads:with-lock-held

(defmacro with-lock ((lock) &body body)
  "Faster replacement for BORDEAUX-THREADS:WITH-LOCK-HELD."
  (with-gensym lock-var
    `(let1 ,lock-var ,lock
       (unwind-protect
            (progn
              (bt:acquire-lock ,lock-var)
              ,@body)
         (bt:release-lock ,lock-var)))))




;;;; ** nowait mutex
;;;;    uses fast atomic ops and read/write memory barriers if available,
;;;;    otherwise falls back on bordeaux-threads locks



(defstruct (mutex (:constructor %make-mutex) (:conc-name))
  #?+fast-mutex
  (mutex-owner nil :type atomic-t)
  #?-fast-mutex
  (mutex-lock (make-lock "MUTEX") :read-only t))





(declaim (ftype (function () mutex) make-mutex)
         (inline make-mutex))

(defun make-mutex ()
  "Create and return a MUTEX."
  (the mutex (%make-mutex)))
    

(declaim (ftype (function (mutex) boolean)  try-acquire-mutex)
         (ftype (function (mutex) t)        release-mutex)
         (inline
           try-acquire-mutex release-mutex))

;;(eval-always
;; (setf sb-vm::*lock-elision* t))

(defun try-acquire-mutex (mutex)
  "Try to acquire MUTEX. Return T if successful,
or NIL if MUTEX was already locked."
  (declare (type mutex mutex))
  #?+fast-mutex
  (mem-write-barrier
    (null
     ;;(sb-transaction:lock-elision-acquire)
     (atomic-compare-and-swap (mutex-owner mutex) nil *current-thread*)))
  #?-fast-mutex
  (bt:acquire-lock (mutex-lock mutex) nil))


(defun release-mutex (mutex)
  "Release MUTEX. Return NIL. Consequences are undefined if MUTEX
is locked by another thread or is already unlocked."
  #?+fast-mutex
  (progn
    ;; (mem-write-barrier)
    ;; (sb-transaction:lock-elision-release)
    (setf (mutex-owner mutex) nil))
  #?-fast-mutex
  (bt:release-lock (mutex-lock mutex)))

   
;;(eval-always
;; (setf sb-vm::*lock-elision* nil))




#?+mutex-owner
(eval-always

  (declaim (ftype (function (mutex) boolean) mutex-is-own-or-free?)
           (inline
             mutex-is-own-or-free?))

  (defun mutex-is-own-or-free? (mutex)
    "Return T if MUTEX is free or locked by current thread.
Return NIL if MUTEX is currently locked by some other thread."

    (let* ((mutex (mem-read-barrier mutex))
           (owner
            (mem-read-barrier

              #?+fast-mutex
              (mutex-owner mutex)

              #?-fast-mutex
              (#.(stmx.lang::get-feature 'bt.lock-owner) (mutex-lock mutex)))))
      
      (or
       (eq owner nil)
       (eq owner *current-thread*)))))
