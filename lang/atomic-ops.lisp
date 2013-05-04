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

;;;; ** atomic operations


#+stmx.have-atomic-ops.sbcl
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

 (defmacro atomic-compare-and-swap (old new place)
   (let ((old-value (gensym "OLD-"))
         (new-value (gensym "NEW-")))
     `(let ((,old-value ,old)
            (,new-value ,new))
        (sb-ext:compare-and-swap ,place ,old-value ,new-value))))


 (defmacro atomic-read-barrier (&body before)
   `(sb-thread:barrier (:read)
      ,@before))

 (defmacro atomic-write-barrier (&body before)
   `(sb-thread:barrier (:write)
      ,@before)))



