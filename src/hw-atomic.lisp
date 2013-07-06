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

;;;; ** global lock to prevent hardware transactions from running.
;;;;    used by software transactions when hw-assisted commit fails.

(declaim (type mutex +hw-transaction-stop-the-world+))
(define-constant-eval-once +hw-transaction-stop-the-world+ (make-mutex))

(defmacro try-acquire-hw-transaction-stop-the-world ()
  "Try to acquire the mutex +HW-TRANSACTION-STOP-THE-WORLD+.
Return T if successful, NIL if failed."
  `(try-acquire-mutex +hw-transaction-stop-the-world+))


(defmacro release-hw-transaction-stop-the-world ()
  "Release the mutex +HW-TRANSACTION-STOP-THE-WORLD+."
  `(release-mutex +hw-transaction-stop-the-world+))


(defmacro hw-transaction-stop-the-world-is-free? ()
  "Return T if the mutex +HW-TRANSACTION-STOP-THE-WORLD+ is free."
  `(mutex-is-free? +hw-transaction-stop-the-world+))




;;;; ** Running hardware transactions



(defmacro %hw-atomic (helper-$-hwtx body fallback)
  "Run BODY in a hardware memory transaction.
If the transaction aborts for any reason, execute FALLBACK."
  
  `(with-hw-tlog (,helper-$-hwtx)
     (if (hw-transaction-begin)

         (progn
           (unless (hw-transaction-stop-the-world-is-free?)
             (hw-transaction-abort))

           (multiple-value-prog1
               ,body
             (hw-transaction-end)))

         (progn
           (global-clock/after-abort)
           ,fallback))))


(defmacro hw-atomic ((&optional (helper-$-hwtx (gensym)))
                     &optional (body nil body?) (fallback body))

  "Run BODY in a hardware memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

If hardware memory transaction aborts for any reason, execute FALLBACK."
  (if body?
      `(%hw-atomic ,helper-$-hwtx ,body ,fallback)
      `(values)))
