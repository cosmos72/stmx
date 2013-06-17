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

;;;; ** atomic operations


#?+(eql atomic-ops :sbcl)
(eval-always
  
  (deftype atomic-num ()
    "ATOMIC-NUM must be a type suitable for ATOMIC-INCF and ATOMIC-DECF.
STMX also assumes it is the same or wider than fixnum."
    'sb-ext:word)

  (defmacro atomic-incf (place &optional (delta 1))
    "Atomically increment PLACE by DELTA. Return _previous_ value of PLACE."
    `(sb-ext:atomic-incf ,place ,delta))

  (defmacro atomic-decf (place &optional (delta 1))
    "Atomically decrement PLACE by DELTA. Return _previous_ value of PLACE."
    `(sb-ext:atomic-decf ,place ,delta))


  (deftype atomic-t ()
    "ATOMIC-T must be a type suitable for ATOMIC-COMPARE-AND-SWAP.
STMX assumes it can hold at least NIL and values of type BORDEAUX-THREADS:THREAD."
    't)

  (defmacro atomic-compare-and-swap (place old new)
    `(sb-ext:compare-and-swap ,place ,old ,new))

  (defmacro atomic-read-barrier (&body before)
    `(sb-thread:barrier (:read)
        ,@before))

  (defmacro atomic-write-barrier (&body before)
    `(sb-thread:barrier (:write)
        ,@before))


  (defmacro atomic-push (obj place)
    "Like PUSH, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works only on places supported by COMPARE-AND-SWAP."
    #?+(symbol sb-ext atomic-push)
    `(sb-ext:atomic-push ,obj ,place)

    #?-(symbol sb-ext atomic-push)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (sb-ext:get-cas-expansion place)
      `(let* (,@(mapcar 'list vars vals)
              (,old ,read-form)
              (,new (cons ,obj ,old)))
         (loop until (eq ,old (setf ,old ,cas-form))
            do (setf (cdr ,new) ,old)
            finally (return ,new)))))


  (defmacro atomic-pop (place)
    "Like POP, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works only on places supported by COMPARE-AND-SWAP."
    #?+(symbol sb-ext atomic-pop)
    `(sb-ext:atomic-pop ,place)

    #?-(symbol sb-ext atomic-pop)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (sb-ext:get-cas-expansion place)
      `(let* (,@(mapcar 'list vars vals))
         (loop for ,old = ,read-form
            for ,new = (cdr ,old)
            until (eq ,old (setf ,old ,cas-form))
            finally (return (car ,old)))))))

#?-atomic-ops
#?+(eql atomic-mem-rw-barriers t)
;; feature 'atomic-mem-rw-barriers = t tells to use the generic implementation,
;; as the features 'atomic-mem-r-barrier and 'atomic-mem-w-barrier
;; will contain the macros that actually implement the read and write barriers.
(eval-always

  (declaim (notinline identity-notinline))
  (defun identity-notinline (arg)
    arg)

  (defmacro atomic-mem-barrier-trivial (&body before)
    "Trivial implementation of memory read/write barriers.
Used for those CPUs that intrinsically guarantee ordering
of read-after-read and write-after-write."
    `(identity-notinline
         (progn
           ,@before)))
 
  ;; generic implementation of memory read barrier
  (defmacro atomic-read-barrier (&body before)
    `(#.(stmx.lang::get-feature 'atomic-mem-r-barrier)
       ,@before))

  ;; generic implementation of memory read barrier
  (defmacro atomic-write-barrier (&body before)
    `(#.(stmx.lang::get-feature 'atomic-mem-w-barrier)
       ,@before)))


;; avoid "unexpected EOF" compiler error
;; if atomic-ops and atomic-mem-rw-barriers are both undefined
nil


