;; -*- lisp -*-

;; This file is part of SB-TRANSACTION.
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


(in-package :sb-transaction)


(declaim (ftype (function () fixnum)        transaction-begin)
         (ftype (function () (integer 0 0)) transaction-end)
         (ftype (function () (integer 0 0)) transaction-abort)
         (ftype (function () boolean)       transaction-running-p)
         (inline transaction-begin
                 transaction-end
                 transaction-abort
                 transaction-running-p))




(defun transaction-begin ()
  "Start a hardware memory transaction.
Return +transaction-started+ if transaction started successfully,
otherwise return code of the error that caused the transaction to abort.

Invoking TRANSACTION-BEGIN while there is already a running hardware
memory transaction has implementation-dependent effects."
  (the (values fixnum  &optional)
    ;; (%transaction-begin)
    (sb-c::%primitive %xbegin)))


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
  #+never (%transaction-end)
  (sb-c::%primitive %xend)
  0)



(declaim (type fixnum +transaction-abort+))

(defconstant +transaction-abort+ #x1000001
  "Value returned by (transaction-begin) if the transaction was manually aborted
by calling (transaction-abort).
It is an implementation-dependent fixnum, different from +transaction-started+
and from all error codes indicating a spontaneous abort.")


(defmacro transaction-abort-macro (&optional (err-code (ash +transaction-abort+ -24)))
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
  `(progn
     (sb-c::%primitive %xabort ,err-code)
     0))



(defun transaction-abort ()
  "Voluntarily abort a hardware memory transaction
with an error-code equal to +transaction-abort+.

If a transaction is in progress, TRANSACTION-ABORT does not return normally:
execution is resumed at the instruction immediately after the outermost
TRANSACTION-BEGIN.

If called without an active transaction, TRANSACTION-ABORT returns normally
with an implementation-dependent value."
  (transaction-abort-macro))





(defun %transaction-running-p ()
  (%transaction-running-p))

(defun transaction-running-p ()
  "Return T if a hardware memory transaction
is currently in progress, otherwise return NIL."
  (%transaction-running-p))


