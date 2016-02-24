;; -*- lisp -*-

;; This file is part of SB-TRANSACTION.
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


(in-package :sb-transaction)

;; SBCL >= 1.3.2 requires x86-64 CPU instructions to be defined
;; in package :sb-x86-64-asm due to package locks.
;;
;; older SBCL versions are more lenient, and allow defining
;; x86-64 CPU instructions either in :sb-vm or
;; (with more difficulty) in other packages
;;
#+#.(sb-transaction::compile-if-package :sb-x86-64-asm)
(in-package :sb-x86-64-asm)

#-#.(sb-transaction::compile-if-package :sb-x86-64-asm)
(in-package :sb-vm)







(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; utilities copied from sbcl/src/compiler/x86-64/insts.lisp:
  ;; BYTE, TWO-BYTES, THREE-BYTES, DISPLACEMENT
  ;;
  ;; They are not needed in package :sb-x86-64-asm,
  ;; and DISPLACEMENT does not even compile there
  #-#.(sb-transaction::compile-if-package :sb-x86-64-asm)
  (progn
    (sb-disassem:define-instruction-format (byte 8 :default-printer '(:name))
	(op :field (byte 8 0)))
  
    (sb-disassem:define-instruction-format (two-bytes   16 :default-printer '(:name))
	(op :fields (list (byte 8 0) (byte 8 8))))
 
    (sb-disassem:define-instruction-format (three-bytes 24 :default-printer '(:name))
	(op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))

    (sb-disassem:define-arg-type displacement
	:sign-extend t
	:use-label #'offset-next
	:printer (lambda (value stream dstate)
		   (sb-disassem:maybe-note-assembler-routine value nil dstate)
		   (print-label value stream dstate))))


  ;; old SBCLs cannot disassemble instructions longer than 32 bits,
  ;; so we fake it by using a prefilter to read the offset.
  #+#.(sb-transaction::compile-if-sbcl-disassem<=32-bit)
  (sb-disassem:define-instruction-format (xbegin
					  16 :default-printer '(:name :tab label))
      (op  :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
    (label :type 'displacement
	   :prefilter (lambda (value dstate)
			(declare (ignore value)) ; always nil anyway
			(sb-disassem:read-signed-suffix 32 dstate))))

  #-#.(sb-transaction::compile-if-sbcl-disassem<=32-bit)
  ;; recent SBCLs can disassemble instructions longer than 32 bits
  (sb-disassem:define-instruction-format (xbegin
					  48 :default-printer '(:name :tab label))
      (op  :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
    (label :field (byte 32 16) :type 'displacement))

  


  (sb-disassem:define-instruction-format (xabort
					  24 :default-printer '(:name :tab imm))
      (op    :fields (list (byte 8 0) (byte 8 8)) :value '(#xc6 #xf8))
    (imm   :field (byte 8 16)))


;;;; ** low-level: assemble machine-language instructions

  (defun emit-dword-displacement-backpatch (segment target)
    (sb-vm::emit-back-patch segment
     4
     (lambda (segment posn)
       (let ((disp (- (sb-vm::label-position target) (+ 4 posn))))
         (sb-int:aver (<= #x-80000000 disp #x7fffffff))
         (let ((disp (the (integer #x-80000000 #x7fffffff) disp)))
           (sb-vm::emit-byte segment (ldb (byte 8  0) disp))
           (sb-vm::emit-byte segment (ldb (byte 8  8) disp))
           (sb-vm::emit-byte segment (ldb (byte 8 16) disp))
           (sb-vm::emit-byte segment (ldb (byte 8 24) disp)))))))
                     

;;; ** HLE - hardware lock elision

  (sb-vm::define-instruction xacquire (segment) ;; same byte as repne/repnz
    (:emitter
     (sb-vm::emit-byte segment #xf2)))

  (sb-vm::define-instruction xrelease (segment) ;; same byte as rep/repe/repz
    (:emitter
     (sb-vm::emit-byte segment #xf3)))



;;; ** RTM - restricted memory transaction

  (sb-vm::define-instruction xbegin (segment &optional where)
    (:printer xbegin ())
    (:emitter
     (sb-vm::emit-byte segment #xc7)
     (sb-vm::emit-byte segment #xf8)
     (if where
	 ;; emit 32-bit, signed relative offset for where
	 (emit-dword-displacement-backpatch segment where)
	 ;; nowhere to jump: simply jump to the next instruction
	 (sb-vm::emit-skip segment 4 0))))


  (sb-vm::define-instruction xend (segment)
    (:printer three-bytes ((op '(#x0f #x01 #xd5))))
    (:emitter
     (sb-vm::emit-byte segment #x0f)
     (sb-vm::emit-byte segment #x01)
     (sb-vm::emit-byte segment #xd5)))


  (sb-vm::define-instruction xabort (segment reason)
    (:printer xabort ())
    (:emitter
     (sb-int:aver (<= 0 reason #xff))
     (sb-vm::emit-byte segment #xc6)
     (sb-vm::emit-byte segment #xf8)
     (sb-vm::emit-byte segment reason)))


  (sb-vm::define-instruction xtest (segment)
    (:printer three-bytes ((op '(#x0f #x01 #xd6))))
    (:emitter
     (sb-vm::emit-byte segment #x0f)
     (sb-vm::emit-byte segment #x01)
     (sb-vm::emit-byte segment #xd6)))


) ; EVAL-WHEN
