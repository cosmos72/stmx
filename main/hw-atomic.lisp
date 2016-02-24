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


(in-package :stmx)

(enable-#?-syntax)


;;;; ** Running hardware transactions

(defconstant +hw-atomic-max-attempts+ 10)

(defmacro %hw-atomic2 ((&key hw-write-version err
                             (test-for-running-tx? t)
                             (update-stat :hwtx))
                       body fallback)
  "Run BODY in a hardware memory transaction.
If the transaction aborts, retry it as long as it has chances to succeed.
If it has no chances to succeed, execute FALLBACK.
Warning: if a transaction is already running, execute BODY inside it"

  (let ((tvar-write-version (or hw-write-version (gensym (symbol-name 'tvar-write-version))))
        (err (or err (gensym (symbol-name 'err)))))
    
    (with-gensyms (tx-begin tx-fallback attempts)
      `(cond
         ,@(if test-for-running-tx?
               `(((hw-transaction?) (with-hwtx ,body))
                 ((sw-transaction?) (with-swtx ,body)))
               `())
         (t
          (prog ((,err 0)
                 (,attempts +hw-atomic-max-attempts+)
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
                       (with-hwtx ,body)
                     (hw-transaction-end)
                     ,(if (eq update-stat :swtx)
                          `(global-clock/sw/stat-committed)
                          `(global-clock/hw/stat-committed))))))

             (unless (zerop (decf (the fixnum ,attempts)))
               (when (hw-transaction-rerun-may-succeed? ,err)
                 ;;(maybe-yield-before-rerun)
                 (go ,tx-begin)))

             ,(if (eq update-stat :swtx)
                  `(global-clock/sw/stat-aborted)
                  `(global-clock/hw/stat-aborted))
             
             ,tx-fallback
             (return ;; returns from (prog ...)
               ,fallback)))))))


(defmacro hw-atomic2 ((&key hw-write-version err
                            (test-for-running-tx? t)
                            (update-stat :hwtx))
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
      `(%hw-atomic2 (:hw-write-version ,hw-write-version :err ,err
                                       :test-for-running-tx? ,test-for-running-tx?
                                       :update-stat ,update-stat)
                    ,body
                    ,fallback)
      `(values)))


