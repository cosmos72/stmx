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

;;(defconstant +hw-atomic-max-attempts+ 3)

(defmacro %hw-atomic2 ((&optional tvar-write-version &key err (test-for-running-tx? t))
                       body fallback)
  "Run BODY in a hardware memory transaction.
If the transaction aborts, retry it as long as it has chances to succeed.
If it has no chances to succeed, execute BODY in a software memory transaction."

  (let ((tvar-write-version (or tvar-write-version (gensym (symbol-name 'tvar-write-version))))
        (err (or err (gensym (symbol-name 'err)))))
    
    (with-gensyms (tx-begin tx-fallback)
      `(cond
         ,@(if test-for-running-tx? `(((transaction?) ,body)) `())
         (t
          (prog ((,err 0)
                 ;; create a a thread-local binding for *hw-tlog-write-version*
                 (*hw-tlog-write-version* +invalid-version+))
             
             (unless (zerop (global-clock/get-nohw-counter))
               (go ,tx-fallback))
             
             ,tx-begin
             (setf ,err (hw-transaction-begin))
             (when (= ,err +hw-transaction-started+)
               ;; hardware transactions are currently incompatible
               ;; with software-only transaction commits :-(
               (unless (zerop (global-clock/get-nohw-counter))
                 (hw-transaction-abort))

               (let ((,tvar-write-version
                      (setf *hw-tlog-write-version*
                            (global-clock/hw/start-write (global-clock/hw/start-read)))))
                 (declare (ignorable ,tvar-write-version))
                  

                 (return ;; returns from (prog ...)
                   (multiple-value-prog1
                       ,body
                     (hw-transaction-end)
                     (global-clock/hw/stat-committed)))))

             (when (hw-transaction-rerun-may-succeed? ,err)
               (go ,tx-begin))

             (global-clock/hw/stat-aborted)
             
             ,tx-fallback
             (return ;; returns from (prog ...)
               ,fallback)))))))


(defmacro hw-atomic2 ((&optional tvar-write-version &key err (test-for-running-tx? t))
                     &optional (body nil body?) fallback)
  "Run BODY in a hardware memory transaction. All changes to transactional memory
will be visible to other threads only after BODY returns normally (commits).
If BODY signals an error, its effects on transactional memory are rolled back
and the error is propagated normally.
Also, no work-in-progress transactional memory will ever be visible to other
threads.

If hardware memory transaction aborts for a conflict, rerun it.
If it fails for some other reason, execute FALLBACK."
  (if body?
      `(%hw-atomic2 (,tvar-write-version :err ,err :test-for-running-tx? ,test-for-running-tx?)
                   ,body ,fallback)
      `(values)))


