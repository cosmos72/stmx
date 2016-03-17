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


;; SBCL >= 1.3.3 does NOT support defining new CPU instruction formats,
;; we can only hope that it already contains the code to disassemble
;; Intel TSX CPU instructions xbegin, xend, xtest, xabort.
;;
;; SBCL >= 1.3.2 requires CPU instructions to be defined
;; in package :sb-x86-64-asm (for 64-bit SBCL) or :sb-x86-asm (for 32-bit SBCL)
;; due to package locks.
;;
;; older SBCL versions are more lenient, and allow defining CPU instructions
;; either in :sb-vm or (with more difficulty) in any other package
;;
(in-package #.stmx.asm:+impl-package+) ;; one of :sb-x86-64-asm :sb-x86-asm or :sb-vm


#+#.(stmx.asm::compile-if-symbol :sb-disassem :define-instruction-format)
(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; utilities copied from sbcl/src/compiler/x86-64/insts.lisp:
  ;; BYTE, TWO-BYTES, THREE-BYTES, DISPLACEMENT
  ;;
  ;; They are not needed in packages :sb-x86-64-asm or :sb-x86-asm
  ;; and DISPLACEMENT does not even compile there
  #+#.(stmx.asm::compile-if-package :sb-vm)
  (progn
    (sb-disassem:define-instruction-format (byte 8 :default-printer '(:name))
	(op :field (byte 8 0)))
  
    (sb-disassem:define-instruction-format (two-bytes   16 :default-printer '(:name))
	(op :fields (list (byte 8 0) (byte 8 8))))
 
    (sb-disassem:define-instruction-format (three-bytes 24 :default-printer '(:name))
	(op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))

    (sb-disassem:define-arg-type displacement
	:sign-extend t
	:use-label #' #.(stmx.asm:find-symbol* 'offset-next)
	:printer (lambda (value stream dstate)
		   (sb-disassem:maybe-note-assembler-routine value nil dstate)
		   (#.(stmx.asm:find-symbol* 'print-label) value stream dstate))))


  ;; old SBCLs cannot disassemble instructions longer than 32 bits,
  ;; so we fake it by using a prefilter to read the offset.
  #+#.(stmx.asm::compile-if-sbcl-disassem<=32-bit)
  (sb-disassem:define-instruction-format (xbegin
					  16 :default-printer '(:name :tab label))
      (op  :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
    (label :type '#.(stmx.asm:find-symbol* 'displacement)
	   :prefilter (lambda (value dstate)
			(declare (ignore value)) ; always nil anyway
			(sb-disassem:read-signed-suffix 32 dstate))))
  
  #-#.(stmx.asm::compile-if-sbcl-disassem<=32-bit)
  ;; recent SBCLs can disassemble instructions longer than 32 bits
  (sb-disassem:define-instruction-format (xbegin
					  48 :default-printer '(:name :tab label))
      (op  :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
    (label :field (byte 32 16) :type '#.(stmx.asm:find-symbol* 'displacement)))
  

  (sb-disassem:define-instruction-format (xabort
					  24 :default-printer '(:name :tab imm))
      (op    :fields (list (byte 8 0) (byte 8 8)) :value '(#xc6 #xf8))
    (imm   :field (byte 8 16)))

) ;; EVAL-WHEN





;;;; ** low-level: assemble machine-language instructions

;; if instructions are already defined (for example in SBCL core),
;; do not overwrite them: we risk redefining them WITHOUT :printer

;;; ** HLE - hardware lock elision, not actually used by STMX

#-#.(stmx.asm::compile-if-instruction-defined 'xacquire)
(sb-vm::define-instruction xacquire (segment) ;; same byte as repne/repnz
  (:emitter
   (sb-vm::emit-byte segment #xf2)))


#-#.(stmx.asm::compile-if-instruction-defined 'xrelease)
(sb-vm::define-instruction xrelease (segment) ;; same byte as rep/repe/repz
  (:emitter
   (sb-vm::emit-byte segment #xf3)))


;;; ** RTM - restricted memory transaction

#-#.(stmx.asm::compile-if-instruction-defined 'xbegin)
(defun emit-dword-displacement-backpatch (segment target &optional (n-extra 0))
  ;; N-EXTRA is how many more instruction bytes will follow, to properly compute
  ;; the displacement from the beginning of the next instruction to TARGET.
  (sb-vm::emit-back-patch segment
   4
   (lambda (segment posn)
     (let ((disp (- (sb-vm::label-position target) (+ 4 posn n-extra))))
       (sb-int:aver (<= #x-80000000 disp #x7fffffff))
       (sb-vm::emit-byte segment (ldb (byte 8  0) disp))
       (sb-vm::emit-byte segment (ldb (byte 8  8) disp))
       (sb-vm::emit-byte segment (ldb (byte 8 16) disp))
       (sb-vm::emit-byte segment (ldb (byte 8 24) disp))))))


#-#.(stmx.asm::compile-if-instruction-defined 'xbegin)
(sb-vm::define-instruction xbegin (segment &optional where)
  #+#.(stmx.asm::compile-if-symbol :sb-disassem :define-instruction-format)
  (:printer xbegin ())
  (:emitter
   (sb-vm::emit-byte segment #xc7)
   (sb-vm::emit-byte segment #xf8)
   (if where
       ;; emit 32-bit, signed relative offset for where
       (emit-dword-displacement-backpatch segment where)
       ;; nowhere to jump: simply jump to the next instruction
       (sb-vm::emit-skip segment 4 0))))


#-#.(stmx.asm::compile-if-instruction-defined 'xend)
(sb-vm::define-instruction xend (segment)
  #+#.(stmx.asm::compile-if-symbol :sb-disassem :define-instruction-format)
  (:printer three-bytes ((op '(#x0f #x01 #xd5))))
  (:emitter
   (sb-vm::emit-byte segment #x0f)
   (sb-vm::emit-byte segment #x01)
   (sb-vm::emit-byte segment #xd5)))


#-#.(stmx.asm::compile-if-instruction-defined 'xabort)
(sb-vm::define-instruction xabort (segment reason)
  #+#.(stmx.asm::compile-if-symbol :sb-disassem :define-instruction-format)
  (:printer xabort ())
  (:emitter
   (sb-int:aver (<= 0 reason #xff))
   (sb-vm::emit-byte segment #xc6)
   (sb-vm::emit-byte segment #xf8)
   (sb-vm::emit-byte segment reason)))


#-#.(stmx.asm::compile-if-instruction-defined 'xtest)
(sb-vm::define-instruction xtest (segment)
  #+#.(stmx.asm::compile-if-symbol :sb-disassem :define-instruction-format)
  (:printer three-bytes ((op '(#x0f #x01 #xd6))))
  (:emitter
   (sb-vm::emit-byte segment #x0f)
   (sb-vm::emit-byte segment #x01)
   (sb-vm::emit-byte segment #xd6)))
