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


(in-package :stmx)

(enable-#?-syntax)

;;;; ** Running hybrid SW/HW transactions

(defmacro atomic (&rest body)
  "Main entry point for STMX.

Run BODY in a memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

A memory transaction can also retry: in such case ATOMIC will abort it,
wait until some of the value read by the transaction have changed,
then re-run the transaction from the beginning.

Since STMX transactions do not lock memory, it is possible for different
transactions to try to update the same memory (almost) simultaneously.
In such case, the conflict is detected when they try to commit or rollback,
and only one conflicting transaction is allowed to commit:
all others are immediately re-run again from the beginning,
also ignoring any error they may have signalled.

For this reason, a transaction ABSOLUTELY MUST NOT perform any irreversible
operation such as INPUT/OUTPUT: the result would be that I/O is executed
multiple times, or executed even when it should not!
Irreversible operations MUST be performed OUTSIDE transactions,
for example by queueing them into transactional memory that another thread
will consume and then, OUTSIDE transactions, actually perform them.

For how to create transactional memory, see TRANSACTIONAL or TVAR.
For another way to run transactions, see also TRANSACTION.
For advanced features inside transactions, see RETRY, ORELSE, NONBLOCKING,
    BEFORE-COMMIT and AFTER-COMMIT.

For pre-defined transactional classes, see the package STMX.UTIL"

#+never
 "Run BODY in a hardware memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

If hardware memory transaction aborts for a conflict, rerun it.
If it fails for some other reason, execute BODY in a software memory transaction."

  (if body
      #?+hw-transactions
      (let ((form `(block nil (locally ,@body))))
        `(%hw-atomic2 () ,form (%run-sw-atomic (lambda () ,form))))
      #?-hw-transactions
      `(sw-atomic ,@body)

      `(values)))


(defmacro fast-atomic (&rest body)
  "Possibly slightly faster variant of ATOMIC.

On systems supporting hardware transactions (as of July 2013, very few systems
support them), FAST-ATOMIC and ATOMIC are identical.
On other systems, multiple nested FAST-ATOMIC forms may be slightly faster than
multiple nested ATOMIC blocks, at the price of compiling BODY more than once."
  #?+hw-transactions
  `(atomic ,@body)

  #?-hw-transactions
  (if body
      (let ((form `(block nil (locally ,@body))))
	`(if (transaction?)
	     ,form
	     (%run-sw-atomic (lambda () ,form))))
      `(values)))



(declaim (inline run-atomic))

(defun run-atomic (tx)
  "Function equivalent of the ATOMIC macro.

Run the function TX inside a memory transaction.
If the transaction is invalid (conflicts) re-run TX immediately, ignoring
any error it may signal.

Otherwise, commit if TX returns normally, or rollback if it signals an error.

Finally, if TX called (retry), re-run it after at least some of the
transactional memory it read has changed."

  (declare (type function tx))

  #?+hw-transactions
  (%hw-atomic2 () (funcall tx) (%run-sw-atomic tx))

  #?-hw-transactions
  (run-sw-atomic tx))

