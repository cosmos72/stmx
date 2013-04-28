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

;;;; ** "slow" read-write lock

#-stmx-have-fast-lock
(eval-always

  (defstruct (lock-rw (:constructor %make-lock-rw) (:conc-name))
    (lock-rw-lock (make-lock "LOCK-RW"))
    (lock-rw-readers 0 :type fixnum))

  (declaim (ftype (function () lock-rw) make-lock-rw)
           (inline make-lock-rw))

  (defun make-lock-rw ()
    "Create and return a LOCK-RW."
    (the lock-rw (%make-lock-rw)))
    

  (declaim (ftype (function (lock-rw) boolean)   try-acquire-lock-rw)
           (ftype (function (lock-rw) boolean)   try-acquire-lock-ro)
           (ftype (function (lock-rw) null)      release-lock-rw)
           (ftype (function (lock-rw) null)      release-lock-ro))


  (defun try-acquire-lock-rw (lock-rw)
    "Try to acquire LOCK-RW for exclusive access. Return T if successful,
or NIL if LOCK-RW was already locked for exclusive or shared access."
    (let ((lock (lock-rw-lock lock-rw)))
    (when (acquire-lock lock nil)
      (or (zerop (lock-rw-readers lock-rw))
          (release-lock lock)))))
  
  (declaim (inline release-lock-rw))
  
  (defun release-lock-rw (lock-rw)
    "Release LOCK-RW previously acquired for exclusive access. Return NIL.
If LOCK-RW was not locked for exclusive access, consequences are undefined."
    (release-lock (lock-rw-lock lock-rw)))


  (defun try-acquire-lock-ro (lock-rw)
    "Try to acquire LOCK-RW for shared access. Return T if successful,
or NIL if LOCK-RW was already locked for exclusive access."
    (let ((lock (lock-rw-lock lock-rw)))
      (when (acquire-lock lock nil)
        (incf (lock-rw-readers lock-rw))
        (release-lock lock)
        t)))

  (defun release-lock-ro (lock-rw)
    "Release LOCK-RW previously acquired for shared access. Return NIL.
or NIL if LOCK-RW was not locked for shared access, consequences are undefined."
    (let ((lock (lock-rw-lock lock-rw)))
      (acquire-lock lock) ;; we must wait... releasing a lock must not fail
      (decf (lock-rw-readers lock-rw))
      (release-lock lock))))
