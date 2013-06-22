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

;; this code is VERY non-portable:
;;
;; it requires SBCL running on a x86-64 CPU that supports RTM,
;; i.e. hardware-assisted memory transactions.
;;
;; As of June 2013, the only x86-64 CPUs supporting RTM are:
;; * Intel Core i5 4570
;; * Intel Core i5 4670
;; * Intel Core i7 4770


(in-package :sbcl-transaction)

;; utilities copy-pasted from sbcl/src/compiler/x86-64/insts.lisp
;; DISPLACEMENT, TWO-BYTES, THREE-BYTES

(sb-disassem:define-arg-type displacement
  :sign-extend t
  :use-label #'sb-vm::offset-next
  :printer (lambda (value stream dstate)
             (sb-disassem:maybe-note-assembler-routine value nil dstate)
             (sb-vm::print-label value stream dstate)))

(sb-disassem:define-instruction-format (two-bytes   16 :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(sb-disassem:define-instruction-format (three-bytes 24 :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))



;;;; ** low-level: disassemble machine-language instructions


(sb-disassem:define-instruction-format (xbegin 16
                                      :default-printer '(:name :tab label))
  (op    :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
  ;; the disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
         :prefilter (lambda (value dstate)
                      (declare (ignore value)) ; always nil anyway
                      (sb-disassem:read-signed-suffix 32 dstate))))



(sb-disassem:define-instruction-format (xabort 24
                                      :default-printer '(:name :tab imm))
  (op    :fields (list (byte 8 0) (byte 8 8)) :value '(#xc6 #xf8))
  (imm   :field (byte 8 16)))



;;;; ** low-level: assemble machine-language instructions


(defun emit-dword-displacement-backpatch (segment target)
  (sb-vm::emit-back-patch segment
                   4
                   (lambda (segment posn)
                     (let ((disp (- (sb-vm::label-position target) (+ 4 posn))))
                       (sb-int:aver (<= #x-80000000 disp #x7fffffff))
                       (sb-vm::emit-byte segment (logand #xff disp))
                       (sb-vm::emit-byte segment (logand #xff (ash disp -8)))
                       (sb-vm::emit-byte segment (logand #xff (ash disp -16)))
                       (sb-vm::emit-byte segment (logand #xff (ash disp -24)))))))
                       



(sb-vm::define-instruction xbegin (segment &optional where)
  (:printer xbegin ()) ;; (op '(#xc7 #xf8))
  (:emitter
   (sb-vm::emit-byte segment #xc7)
   (sb-vm::emit-byte segment #xf8)
   (if where
       ;; emit 32-bit, signed relative offset for where
       (emit-dword-displacement-backpatch segment where)
       ;; no where to jump: simply jump to the next instruction
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





;;;; ** medium-level: define vops


(sb-c:defknown %transaction-begin () (unsigned-byte 32)
    ())

(sb-c:defknown %transaction-end () (values)
    ())

(sb-c:defknown %transaction-abort ((unsigned-byte 8)) (values)
    ())

(sb-c:defknown transaction-running-p () (boolean)
    ())


(sb-c:define-vop (%xbegin)
  (:policy :fast-safe)
  (:translate %transaction-begin)

  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :target r1) eax)
  (:results   (r1 :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::positive-fixnum)
  (:generator 8
   (sb-vm::zeroize eax)
   (sb-assem:inst xbegin)
   (sb-c:move r1 eax)))

   
(sb-c:define-vop (%xend)
  (:policy :fast-safe)
  (:translate %transaction-end)
  ;;(:results   (res :scs (sb-vm::unsigned-reg)))
  ;;(:result-types sb-vm::positive-fixnum)
  (:generator 8
   (sb-assem:inst xend)
   #+never (sb-vm::zeroize res)))

   
(sb-c:define-vop (%xabort)
  (:policy :fast-safe)
  (:translate %transaction-abort)
  (:info err-code)
  (:generator 8
   (sb-assem:inst xabort err-code)))


(sb-c:define-vop (%xtest)
  (:policy :fast-safe)
  (:translate transaction-running-p)
  (:conditional :ne)
  (:generator 8
   (sb-assem:inst xtest)))
    




(declaim (ftype (function () (unsigned-byte 32)) transaction-begin)
         (ftype (function () (integer 0 0))      transaction-end)
         (ftype (function () (integer -1 -1))    transaction-abort)
         (inline transaction-begin
                 transaction-end
                 transaction-abort))

(defun transaction-begin ()
  "Start a hardware-assisted memory transaction.
Return zero if transaction started successfully,
otherwise return code of the error that caused the transaction to abort."
  (the (values (unsigned-byte 32) &optional)
    (sb-c::%primitive %xbegin)))


(defun transaction-end ()
  "Commit a hardware-assisted memory transaction.
Return 0 if commit is successful, otherwise abort the transaction.

In case the transaction is aborted, all effects of code between the outermost
TRANSACTION-BEGIN and the TRANSACTION-END will be rolled back (undone):
execution will resume at the instruction immediately after the outermost
TRANSACTION-BEGIN, in such a way that TRANSACTION-BEGIN will appear to have
returned a non-zero error code (that describes the abort reason)."
  (sb-c::%primitive %xend)
  0)




(defmacro transaction-abort-macro (&optional (err-code 1))
  "Voluntarily abort a hardware-assisted memory transaction with a user-specified
ERR-CODE, which must be a constant between 0 and 255 (default: 1).

If a transaction is in progress, TRANSACTION-ABORT-MACRO does not return normally:
execution is resumed at the instruction immediately after the outermost
TRANSACTION-BEGIN.

If called without an active transaction, TRANSACTION-ABORT-MACRO returns normally
with an implementation-dependent value."
  (unless (typep err-code '(unsigned-byte 8))
    (error 'type-error
           :expected-type '(unsigned-byte 8) :datum err-code))
  `(progn
     (sb-c::%primitive %xabort ,err-code)
     -1))

(defun transaction-abort ()
  "Voluntarily abort a hardware-assisted memory transaction
with an error-code equal to 1.

If a transaction is in progress, TRANSACTION-ABORT does not return normally:
execution is resumed at the instruction immediately after the outermost
TRANSACTION-BEGIN.

If called without an active transaction, TRANSACTION-ABORT returns normally
with an implementation-dependent value."
  (transaction-abort-macro))





(defun transaction-running-p ()
  "Return T if a hardware-assisted memory transaction
is currently in progress, otherwise return NIL."
  (transaction-running-p))



