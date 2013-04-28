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

;;;; ** fast read-write lock

#+stmx-have-fast-lock
(eval-always

 (defstruct (lock-rw (:constructor %make-lock-rw) (:conc-name))
   (lock-rw-counter 0 :type fixnum))


 (declaim (ftype (function () lock-rw) make-lock-rw)
          (inline make-lock-rw))

 (defun make-lock-rw ()
   "Create and return a LOCK-RW."
   (the lock-rw (%make-lock-rw)))
    

 (declaim (ftype (function (lock-rw) boolean)   try-acquire-lock-rw)
          (ftype (function (lock-rw) boolean)   try-acquire-lock-ro)
          (ftype (function (lock-rw) null)      release-lock-rw)
          (ftype (function (lock-rw) null)      release-lock-ro)
          (inline
            try-acquire-lock-rw
            try-acquire-lock-ro
            release-lock-rw
            release-lock-ro))


 (defun try-acquire-lock-rw (lock-rw)
   "Try to acquire LOCK-RW for exclusive access. Return T if successful,
or NIL if LOCK-RW was already locked for exclusive or shared access."
   #+stmx-have-sbcl.atomic-ops
   (sb-thread:barrier (:write)
     (zerop (sb-ext:compare-and-swap (lock-rw-counter lock-rw) 0 1))))


 (defun release-lock-rw (lock-rw)
   "Release LOCK-RW previously acquired for exclusive access. Return NIL.
If LOCK-RW was not locked for exclusive access, consequences are undefined."
   #+stmx-have-sbcl.atomic-ops
   (progn
     (sb-thread:barrier (:write))
     (setf (lock-rw-counter lock-rw) 0)
     nil))

 (defun try-acquire-lock-ro (lock-rw)
   "Try to acquire LOCK-RW for shared access. Return T if successful,
or NIL if LOCK-RW was already locked for exclusive access."
   #+stmx-have-sbcl.atomic-ops
   (sb-thread:barrier (:read))
   (sb-thread:barrier (:write)
     (let ((read-value (lock-rw-counter lock-rw)))
       (loop always (zerop (logand read-value 1)) do
            (let ((swap-value
                   (sb-ext:compare-and-swap (lock-rw-counter lock-rw)
                                            read-value
                                            (+ 2 read-value))))
              (when (= read-value swap-value)
                (return t))
              (setf read-value swap-value))))))

 (defun release-lock-ro (lock-rw)
   "Release LOCK-RW previously acquired for shared access. Return NIL.
or NIL if LOCK-RW was not locked for shared access, consequences are undefined."
   #+stmx-have-sbcl.atomic-ops
   (sb-thread:barrier (:read))
   (sb-thread:barrier (:write)
     (let ((read-value (lock-rw-counter lock-rw)))
       (loop do
            (let ((swap-value
                   (sb-ext:compare-and-swap (lock-rw-counter lock-rw)
                                            read-value
                                            (- read-value 2))))
              (when (= read-value swap-value)
                (return nil))
              (setf read-value swap-value)))))))
