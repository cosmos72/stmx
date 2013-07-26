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
        `(%hw-atomic2 () ,form (%run-atomic (lambda () ,form))))
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
	     (%run-atomic (lambda () ,form))))
      `(values)))

