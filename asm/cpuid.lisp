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



(declaim (ftype (function ((unsigned-byte 32) &optional (unsigned-byte 32))
                          (values (unsigned-byte 32) (unsigned-byte 32)
                                  (unsigned-byte 32) (unsigned-byte 32) &optional))
                 cpuid)
         (inline cpuid))

(defun cpuid (eax &optional (ecx 0))
  (%cpuid eax ecx))



(declaim (ftype (function () boolean) transaction-supported-p))


(defun lock-elision-supported-p ()
  "Test for HLE, i.e. hardware lock elision.
HLE is supported if (cpuid 7) returns ebx with bit 4 set.
If a processor does not support HLE, it will ignore the
new assembler instruction prefixes XACQUIRE and XRELEASE.

As of June 2013, the only x86-64 CPUs supporting HLE are:
* Intel Core i5 4570
* Intel Core i5 4670
* Intel Core i7 4770
Beware: at the time of writing all the known K models, as for example
Intel Core i7 4770K, do **NOT** support HLE."

  (let ((max-cpuid (cpuid 0)))
    (when (>= max-cpuid 7)
      (let ((ebx (nth-value 1 (cpuid 7))))
        (not (zerop (logand ebx #x10)))))))


(defun transaction-supported-p ()
  "Test for RTM, i.e. hardware memory transactions.
RTM is supported if (cpuid 7) returns ebx with bit 11 set.
If a processor does not support HLE, trying to execute
the new assembler instructions XBEGIN, XEND, XABORT and XTEST
will generate faults.

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


#|
(defmacro word->extract-byte (word offset)
  `(ldb (byte 8 (ash (the (integer 0 3) ,offset) 3))
	(the (unsigned-byte 32) ,word)))

(defmacro word->extract-byte (word offset)
  `(logand #xff
	   (ash (the (unsigned-byte 32) ,word)
		(* -8 (the (integer 0 3) ,offset)))))

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

