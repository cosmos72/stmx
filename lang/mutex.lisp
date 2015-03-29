;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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

  #?+(eql bt/with-lock :fast)
  (with-gensym lock-var
    `(let ((,lock-var ,lock))
       (unwind-protect
	    (progn
              (bt:acquire-lock ,lock-var)
              ,@body)
         (bt:release-lock ,lock-var))))

  #?+(eql bt/with-lock :single-thread)
  (declare (ignore lock))
  #?+(eql bt/with-lock :single-thread)
  `(progn
     ,@body)

  #?-(or (eql bt/with-lock :fast) (eql bt/with-lock :single-thread))
  `(bt:with-lock-held (,lock)
     ,@body))


#?+(and bt/lock-owner (not (eql bt/lock-owner :abcl)))
(progn
  (declaim (ftype (function (t) t) lock-owner)
           (inline lock-owner))

 (defun lock-owner (lock)
   "Return the thread owning a lock, or NIL if lock is free."
   (#.(stmx.lang::get-feature 'bt/lock-owner) lock)))




;;;; ** nowait mutex
;;;;    uses fast atomic ops and read/write memory barriers if available,
;;;;    otherwise falls back on bordeaux-threads locks.
;;;;    Tries very hard to also define (mutex-owner) and related functions.


#?+(eql fast-mutex :single-thread)
(progn
  (deftype mutex () 'null)

  (declaim (inline mutex-owner mutex-lock))

  (defun mutex-owner (mutex)
   "Return the thread that locked a mutex, or NIL if mutex is free."
   (declare (type mutex mutex)
	    (ignore mutex))
   nil))

	   

#?-(eql fast-mutex :single-thread)
(defstruct (mutex (:constructor %make-mutex) (:conc-name))
  #?+fast-mutex
  (mutex-owner nil :type atomic-t)
  #?-fast-mutex
  (mutex-lock (make-lock "MUTEX") :read-only t))





(declaim (ftype (function () mutex) make-mutex)
         (inline make-mutex))

(defun make-mutex ()
  "Create and return a MUTEX."
  #?+(eql fast-mutex :single-thread)
  nil
  #?-(eql fast-mutex :single-thread)
  (the mutex (%make-mutex)))
    

(declaim #?-fast-mutex
         (ftype (function (mutex) (values (member t nil :recursion) &optional))
                          try-acquire-mutex/catch-recursion)

         (ftype (function (mutex) (values boolean &optional))  try-acquire-mutex)
         (ftype (function (mutex) t)        release-mutex)
         (inline
           try-acquire-mutex release-mutex))





;; ABCL needs its own magic... defined later
#?+(and (eql mutex-owner :bt/lock-owner) (not (eql bt/lock-owner :abcl)))
(eval-always

 (declaim (ftype (function (mutex) t) mutex-owner)
          (inline mutex-owner))

 (defun mutex-owner (mutex)
   "Return the thread that locked a mutex, or NIL if mutex is free."
   (declare (type mutex mutex))
   
   (lock-owner (mutex-lock mutex))))




;; ABCL needs its own magic... defined later
#?+(and mutex-owner (not (eql bt/lock-owner :abcl))) 
(eval-always

  (declaim (ftype (function (mutex) boolean) mutex-is-free? mutex-is-own? mutex-is-own-or-free?)
           (inline
             mutex-is-free? mutex-is-own? mutex-is-own-or-free?))

  (defun mutex-is-free? (mutex)
    "Return T if MUTEX is free. Return NIL if MUTEX
is currently locked by current thread or some other thread."
    
    (mem-read-barrier)
    (let1 owner (mutex-owner mutex)
      (eq owner nil)))


  (defun mutex-is-own? (mutex)
    "Return T if MUTEX is locked by current thread."

    (mem-read-barrier) ;; remember: MUTEX-OWNER depends on MEM-RW-BARRIERS
    (eq *current-thread* (mutex-owner mutex)))


  (defun mutex-is-own-or-free? (mutex)
    "Return T if MUTEX is free or locked by current thread.
Return NIL if MUTEX is currently locked by some other thread."

    (mem-read-barrier)
    (let1 owner (mutex-owner mutex)
      (or
       (eq owner nil)
       (eq owner *current-thread*)))))




#?+(and mutex-owner (eql bt/lock-owner :abcl)) ;; ABCL needs its own magic
(eval-always

  (defconstant +bt-lock-is-locked+ 
    (java:jmethod "java.util.concurrent.locks.ReentrantLock" "isLocked"))

  (defconstant +bt-lock-is-locked-by-current-thread+ 
    (java:jmethod "java.util.concurrent.locks.ReentrantLock" "isHeldByCurrentThread"))


  (declaim (ftype (function (mutex) boolean) mutex-is-free? mutex-is-own? mutex-is-own-or-free?)
           (inline
             mutex-is-free? mutex-is-own? mutex-is-own-or-free?))

  (defun mutex-is-free? (mutex)
    "Return T if MUTEX is free. Return NIL if MUTEX
is currently locked by current thread or some other thread."

    (let ((jlock (bt::mutex-lock (mutex-lock mutex))))
      (mem-read-barrier)
      (not (java:jcall +bt-lock-is-locked+ jlock))))
  

  (defun mutex-is-own? (mutex)
    "Return T if MUTEX is locked by current thread."

    (let ((jlock (bt::mutex-lock (mutex-lock mutex))))
      (mem-read-barrier)
      (java:jcall +bt-lock-is-locked-by-current-thread+ jlock)))


  (defun mutex-is-own-or-free? (mutex)
    "Return T if MUTEX is free or locked by current thread.
Return NIL if MUTEX is currently locked by some other thread."

    (let ((jlock (bt::mutex-lock (mutex-lock mutex))))
      (mem-read-barrier)
      (or
       (java:jcall +bt-lock-is-locked-by-current-thread+ jlock)
       (not (java:jcall +bt-lock-is-locked+ jlock))))))




(defun try-acquire-mutex (mutex)
  "Try to acquire MUTEX. Return T if successful,
or NIL if MUTEX was already locked."
  (declare (type mutex mutex))
  #?+fast-mutex
  (mem-write-barrier
    (null
     (atomic-compare-and-swap (mutex-owner mutex) nil *current-thread*)))
  #?-fast-mutex
  (bt:acquire-lock (mutex-lock mutex) nil))



#?-fast-mutex
(defun try-acquire-mutex/catch-recursion (mutex)
  "Try to acquire MUTEX. Return T if successful, or :RECURSION if MUTEX was already locked
by current thread, or NIL if MUTEX was already locked by some other thread."
  (declare (type mutex mutex))

  ;; at least on CMUCL, acquiring twice a lock from the same thread
  ;; simply returns nil instead of raising a signal.
  ;; So we use (mutex-is-own?) or (lock-owner) when available

  #?+mutex-owner
  (if (mutex-is-own? mutex)
      :recursion
      (try-acquire-mutex mutex))

  #?-mutex-owner
  (let ((lock (mutex-lock mutex)))

    #?+(and bt/lock-owner (not (eql bt/lock-owner :abcl)))
    (when (eq *current-thread* (lock-owner lock))
      (return-from try-acquire-mutex/catch-recursion :recursion))

    (handler-case
        (bt:acquire-lock lock nil)
      (condition () :recursion))))


(defun release-mutex (mutex)
  "Release MUTEX. Return NIL. Consequences are undefined if MUTEX
is locked by another thread or is already unlocked."
  #?+fast-mutex
  (progn
    (mem-write-barrier)
    (setf (mutex-owner mutex) nil))
  #?-fast-mutex
  (progn
    (bt:release-lock (mutex-lock mutex))
    nil))
