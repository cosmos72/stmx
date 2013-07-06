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

;;;; ** Running hardware transactions



(defmacro %hw-atomic (body fallback)
  "Run BODY in a hardware memory transaction.
If the transaction aborts for any reason, execute FALLBACK."
  
  `(with-hw-tlog
     (if (hw-transaction-begin)

         (multiple-value-prog1
             ,body
           (hw-transaction-end))

         (progn
           (global-clock/after-abort)
           ,fallback))))


(defmacro hw-atomic (&optional (body nil body?) (fallback body))
  "Run BODY in a hardware memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

If hardware memory transaction aborts for any reason, execute FALLBACK."
  (if body?
      `(%hw-atomic ,body ,fallback)
      `(values)))
