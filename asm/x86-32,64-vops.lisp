;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :stmx.asm)


;;;; ** medium-level: define vops

;;; cpuid VOP

(sb-c:define-vop (%cpuid)
  (:policy :fast-safe)
  (:translate %cpuid)

  (:args (eax-val :scs (sb-vm::unsigned-reg) :target eax)
         (ecx-val :scs (sb-vm::unsigned-reg) :target ecx))
  (:arg-types sb-vm::unsigned-num sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :target r1 :from (:argument 0)) eax)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::ecx-offset :target r3 :from (:argument 1)) ecx)
  #+x86-64
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::ebx-offset :target r2) ebx)
  #+x86-64
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::edx-offset :target r4) edx)
  (:results
   (r1 :scs (sb-vm::unsigned-reg))
   (r2 :scs (sb-vm::unsigned-reg))
   (r3 :scs (sb-vm::unsigned-reg))
   (r4 :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num sb-vm::unsigned-num
                 sb-vm::unsigned-num sb-vm::unsigned-num)
  (:generator 8
   (sb-c:move eax eax-val)
   (sb-c:move ecx ecx-val)
   #+x86-64
   (progn
     (sb-assem:inst cpuid)
     (sb-c:move r1 eax)
     (sb-c:move r2 ebx)
     (sb-c:move r3 ecx)
     (sb-c:move r4 edx))
   #-x86-64
   (let ((ebx sb-vm::ebx-tn)
         (edx sb-vm::edx-tn)
         (save-ebx t)
         (save-edx t))
     (dolist (r (list r1 r2 r3 r4))
       (when (sb-c::location= r ebx)
         (setf save-ebx nil))
       (when (sb-c::location= r edx)
         (setf save-edx nil)))
     (when save-edx
       (sb-assem:inst push edx))
     (when save-edx
       (sb-assem:inst push ebx))
     (sb-assem:inst cpuid)
     (sb-assem:inst push edx)
     (sb-assem:inst push ebx)
     (sb-c:move r1 eax)
     (sb-assem:inst pop r2)
     (sb-c:move r3 ecx)
     (sb-assem:inst pop r4)
     (when save-ebx
       (sb-assem:inst pop ebx))
     (when save-edx
       (sb-assem:inst pop edx)))))




;;; HLE vops - hardware lock elision




;;; RTM vops - restricted memory transaction

(declaim (type fixnum +transaction-started+))

(defconstant +transaction-started+ 3
  "Value returned by (transaction-begin) if the transaction is successfully started.
It is an implementation-dependent fixnum, different from all possible transactions
abort error codes.")


(sb-c:define-vop (%xbegin)
  (:policy :fast-safe)
  (:translate %transaction-begin)

  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :target r1) eax)
  (:results   (r1 :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 0
   (sb-assem:inst mov eax +transaction-started+)
   (sb-assem:inst xbegin)
   (sb-c:move r1 eax)))


(sb-c:define-vop (%xend)
  (:policy :fast-safe)
  (:translate %transaction-end)
  (:generator 0
   (sb-assem:inst xend)))


(sb-c:define-vop (%xabort)
  (:policy :fast-safe)
  (:translate %transaction-abort)
  (:info err-code)
  (:generator 0
   (sb-assem:inst xabort err-code)))


(sb-c:define-vop (%xtest)
  (:policy :fast-safe)
  (:translate %transaction-running-p)
  (:conditional :ne)
  (:generator 1
   (sb-assem:inst xtest)))

