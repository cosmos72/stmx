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

(defconstant +hw-atomic-max-attempts+ 3)

(defmacro %hw-atomic ((&optional tvar-write-version err)
                      body fallback)
  "Run BODY in a hardware memory transaction.
If the transaction aborts for any reason, execute FALLBACK."

  (let ((tvar-write-version (or tvar-write-version (gensym (symbol-name 'tvar-write-version))))
        (err (or err (gensym (symbol-name 'err)))))
    (with-gensyms (attempts-left tx-begin tx-fallback)
      `(prog ((,err 0)
              (,attempts-left +hw-atomic-max-attempts+)
              (*hw-tlog-write-version* 0)) ;; thread-local global variable

          (declare (type (integer 0 #.+hw-atomic-max-attempts+) ,attempts-left))

          (unless (zerop (global-clock/get-sw-commits))
            (go ,tx-fallback))

          ,tx-begin
          (setf ,err (hw-transaction-begin))
          (when (= ,err +hw-transaction-started+)
            (let ((,tvar-write-version
                   (setf *hw-tlog-write-version*
                         (global-clock/hw/start-write (global-clock/hw/start-read)))))
              (declare (ignorable ,tvar-write-version))

              ;; hardware transactions are currently incompatible
              ;; with software-only transaction commits :-(
              (unless (zerop (global-clock/get-sw-commits))
                (hw-transaction-abort))

              (return ;; returns from (prog ...)
                (multiple-value-prog1
                    (block nil
                      ,body)
                  (hw-transaction-end)
                  (global-clock/stat-committed)))))

          (unless (zerop (decf ,attempts-left))
            (when (hw-transaction-rerun-may-succeed? ,err)
              (go ,tx-begin)))

          (global-clock/stat-aborted)

          ,tx-fallback
          (return ;; returns from (prog ...)
            (block nil
              ,fallback))))))


(defmacro hw-atomic ((&optional tvar-write-version err)
                     &optional (body nil body?) (fallback body))

  "Run BODY in a hardware memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

If hardware memory transaction aborts for any reason, execute FALLBACK."
  (if body?
      `(%hw-atomic (,tvar-write-version ,err) ,body ,fallback)
      `(values)))


