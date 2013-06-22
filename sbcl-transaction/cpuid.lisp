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


;;;; * SBCL-TRANSACTION

;; this code is VERY non-portable: it requires SBCL running on x86-64

(in-package :sbcl-transaction)

(eval-when (:compile-toplevel :load-toplevel)

  (sb-c:defknown %cpuid
      ;;arg-types
      ((unsigned-byte 32) (unsigned-byte 32))
      ;;result-type
      (values (unsigned-byte 32) (unsigned-byte 32)
              (unsigned-byte 32) (unsigned-byte 32))
      ())


  (sb-c:define-vop (%cpuid)
    (:policy :fast-safe)
    (:args (eax-val :scs (sb-vm::unsigned-reg) :target eax)
           (ecx-val :scs (sb-vm::unsigned-reg) :target ecx))
    (:arg-types sb-vm::positive-fixnum sb-vm::positive-fixnum)
    (:translate %cpuid)

    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :target r1 :from (:argument 0)) eax)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::ebx-offset :target r2) ebx)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::ecx-offset :target r3 :from (:argument 1)) ecx)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::edx-offset :target r4) edx)
    (:results
     (r1 :scs (sb-vm::unsigned-reg))
     (r2 :scs (sb-vm::unsigned-reg))
     (r3 :scs (sb-vm::unsigned-reg))
     (r4 :scs (sb-vm::unsigned-reg)))
    (:result-types sb-vm::positive-fixnum sb-vm::positive-fixnum
                   sb-vm::positive-fixnum sb-vm::positive-fixnum)
    (:generator 8
     (sb-c:move eax eax-val)
     (sb-c:move ecx ecx-val)

     (sb-assem:inst cpuid)

     (sb-c:move r1 eax)
     (sb-c:move r2 ebx)
     (sb-c:move r3 ecx)
     (sb-c:move r4 edx))))

   


(defun %cpuid (eax ecx)
  (%cpuid eax ecx))

(declaim (inline cpuid))
(defun cpuid (eax &optional (ecx 0))
  (declare (type (unsigned-byte 32) eax ecx))
  (the (values (unsigned-byte 32) (unsigned-byte 32)
               (unsigned-byte 32) (unsigned-byte 32) &optional)
    (sb-c::%primitive %cpuid eax ecx)))


#|
(defmacro word->extract-byte (word offset)
  `(ldb (byte 8 (ash ,offset 3)) (the (unsigned-byte 32) ,word)))

(defun cpuid-vendor ()
  (multiple-value-bind (eax ebx ecx edx) (cpuid 0)
    (declare (ignore eax))
    (let ((s (make-string 12))
          (i 0))
      (declare (type (mod 13) i))
      (dolist (word (list ebx edx ecx))
        (dotimes (j 4)
          (let ((code (word->extract-byte word j)))
            (setf (char s i) (code-char code))
            (incf i))))
      s)))

(defun cpuid-processor-brand ()
  (let ((s (make-string (* 32 3)))
        (i 0))
    (dolist (n '(#x80000002 #x80000003 #x80000004))
      (declare (type (unsigned-byte 32) n))
      (multiple-value-bind (eax ebx ecx edx) (cpuid n)
	(dolist (word (list eax ebx ecx edx))
          (dotimes (j 4)
            (let ((code (word->extract-byte word j)))
              (unless (zerop code)
                (setf (char s i) (code-char code))
                (incf i)))))))
    (subseq s 0 i)))
|#


(defun cpuid-transaction-supported-p ()
  "Test for RTM, i.e. hardware-assisted memory transactions.
RTM is supported if (cpuid 7) returns ebx with bit 11 set.
As of June 2013, the only x86-64 CPUs supporting RTM are:
* Intel Core i5 4570
* Intel Core i5 4670
* Intel Core i7 4770
Beware: at the time of writing all the known K models, as for example
Intel Core i7 4770K, do **NOT** support RTM."

  (let ((max-cpuid (cpuid 0)))
    (when (>= max-cpuid 7)
      (let ((ebx (nth-value 1 (cpuid 7))))
        (not (zerop (logand ebx #x800)))))))


;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (when (cpuid-transaction-supported-p)
;    (pushnew :memory-transaction-vops *features*)))
