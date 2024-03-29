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


(declaim (ftype (function () (values (unsigned-byte 32) &optional)) transaction-begin)
         (ftype (function () (values                    &optional)) transaction-end)
         (ftype (function () (values                    &optional)) transaction-abort)
         (ftype (function () (values boolean            &optional)) transaction-running-p)
         (ftype (function (fixnum) (values boolean      &optional)) transaction-rerun-may-succeed-p)
         (inline transaction-begin
                 transaction-end
                 transaction-abort
                 transaction-running-p
                 transaction-rerun-may-succeed-p))


(declaim (ftype (function () boolean) transaction-supported-p))

(defun transaction-supported-p ()
  "Test for RTM, i.e. hardware memory transactions.
RTM is supported on Intel CPUs if (cpuid 7) returns ebx with bit 11 set.
If a processor does not support HLE, trying to execute
the assembler instructions XBEGIN, XEND, XABORT and XTEST
will generate faults."
  nil)


(defun transaction-begin ()
  "Start a hardware memory transaction.
Return +transaction-started+ if transaction started successfully,
otherwise return code of the error that caused the transaction to abort.

Invoking TRANSACTION-BEGIN while there is already a running hardware
memory transaction has implementation-dependent effects."
  (the fixnum (1- +transaction-started+)))


(defun transaction-end ()
  "Commit a hardware memory transaction.
Return normally (with an implementation-dependent value) if commit is successful,
otherwise abort the transaction.

In case the transaction is aborted, all effects of code between TRANSACTION-BEGIN
and TRANSACTION-END are rolled back (undone):
execution resumes at the instruction immediately after TRANSACTION-BEGIN,
in such a way that TRANSACTION-BEGIN will appear to have returned
a non-zero error code (that describes the abort reason).

Invoking TRANSACTION-END without a running hardware memory transaction
has undefined consequences."
  (values))



(declaim (type fixnum +transaction-user-abort+))

(defconstant +transaction-user-abort+ #x1000001
  "Value returned by (transaction-begin) if the transaction was manually aborted
by calling (transaction-abort).
It is an implementation-dependent fixnum, different from +transaction-started+
and from all error codes indicating a spontaneous abort.")


(defmacro transaction-abort-macro (&optional (err-code (ash +transaction-user-abort+ -24)))
  "Immediately abort a hardware memory transaction with a user-specified
ERR-CODE, which must be a constant between 0 and 255 (default: 1).
Note: the value returned by (transaction-begin) will also contain \"somewhere\"
the bits of ERR-CODE, but will have a different value.
See Intel x86-64 CPU instruction reference manual, section TSX, for details.

If a transaction is in progress, TRANSACTION-ABORT-MACRO does not return normally:
execution is resumed at the instruction immediately after the outermost
TRANSACTION-BEGIN.

If called without a running transaction, TRANSACTION-ABORT-MACRO returns normally
with an implementation-dependent value."
  (unless (typep err-code '(unsigned-byte 8))
    (error 'type-error
           :expected-type '(unsigned-byte 8) :datum err-code))
  `(values))



(defun transaction-abort ()
  "Voluntarily abort a hardware memory transaction
with an error-code equal to +transaction-user-abort+.

If a transaction is in progress, TRANSACTION-ABORT does not return normally:
execution is resumed at the instruction immediately after the outermost
TRANSACTION-BEGIN.

If called without an active transaction, TRANSACTION-ABORT returns normally
with an implementation-dependent value."
  (transaction-abort-macro))


(defun transaction-running-p ()
  "Return T if a hardware memory transaction
is currently in progress, otherwise return NIL."
  nil)




(defun transaction-rerun-may-succeed-p (err-code)
  "If ERR-CODE is the result returned by (TRANSACTION-BEGIN) of an *aborted* transaction,
return T if re-running the same transaction has a possibility to succeed,
i.e. if the abort reason was temporary (as for example a conflict with another thread).
Return NIL if re-running the same transaction has no possibility to succeed."
  (declare (type fixnum err-code)
           (ignore err-code))
  nil)
