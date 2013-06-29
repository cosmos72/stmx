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
  #?+fast-lock
  (mutex-owner nil :type atomic-t)
  #?-fast-lock
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


(defun try-acquire-mutex (mutex)
  "Try to acquire MUTEX. Return T if successful,
or NIL if MUTEX was already locked."
  (declare (type mutex mutex))
  #?+fast-lock
  (mem-write-barrier
    (null
     (atomic-compare-and-swap (mutex-owner mutex) nil *current-thread*)))
  #?-fast-lock
  (bt:acquire-lock (mutex-lock mutex) nil))


(defun release-mutex (mutex)
  "Release MUTEX. Return NIL. Consequences are undefined if MUTEX
is locked by another thread or is already unlocked."
  #?+fast-lock
  (progn
    (mem-write-barrier)
    (setf (mutex-owner mutex) nil))
  #?-fast-lock
  (progn
    (bt:release-lock (mutex-lock mutex))
    nil))

   


#?-fast-lock #?+mutex-owner
(eval-always

 (declaim (ftype (function (mutex) t) mutex-owner)
          (inline mutex-owner))

 #?-(eql bt.lock-owner :abcl) ;; ABCL needs its own magic
 (defun mutex-owner (mutex)
   "Return the thread that locked a mutex, or NIL if mutex is free."
   (declare (type mutex mutex))
   
   (#.(stmx.lang::get-feature 'bt.lock-owner) (mutex-lock mutex))))




#?+(and mutex-owner (not (eql bt.lock-owner :abcl))) ;; ABCL needs its own magic
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

    (mem-read-barrier)
    (let1 owner (mutex-owner mutex)
      (eq owner *current-thread*)))


  (defun mutex-is-own-or-free? (mutex)
    "Return T if MUTEX is free or locked by current thread.
Return NIL if MUTEX is currently locked by some other thread."

    (mem-read-barrier)
    (let1 owner (mutex-owner mutex)
      (or
       (eq owner nil)
       (eq owner *current-thread*)))))




#?+(and mutex-owner (eql bt.lock-owner :abcl)) ;; ABCL needs its own magic
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



;; avoid "unexpected end-of-file" compile errors
;; if none of the above conditional features is present
nil
