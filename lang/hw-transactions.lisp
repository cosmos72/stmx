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


(in-package :stmx.lang)

(enable-#?-syntax)

;;;; ** hardware transactions. need CPU support,
;;;; see package sb-transaction and also stmx/lang/features-detect.lisp


#?+(eql hw-transactions :sb-transaction)
(progn
  
  (defconstant +hw-transaction-started+ sb-transaction:+transaction-started+)

  (defmacro hw-transaction-supported? ()
    "Return T if the CPU supports hardware memory transactions
and there is a compiler extension to use them,
otherwise return NIL."
    `(sb-transaction:transaction-supported-p))

  (defmacro hw-transaction-begin ()
    "Start a hardware memory transaction. Return +transaction-started+
if transaction runs successfully, otherwise return abort reason."
    `(sb-transaction:transaction-begin))

  (defmacro hw-transaction-running? ()
    "Return T if a hardware memory transaction is in progress."
    `(sb-transaction:transaction-running-p))

  (defmacro hw-transaction-abort ()
    "Abort a hardware memory transaction currently in progress.
Causes a rollback of *all* transaction effects, execution resumes
at (hw-transaction-begin) by returning abort reason."
    `(sb-transaction:transaction-abort))

  (defmacro hw-transaction-end ()
    "Try to commit a hardware memory transaction currently in progress.
If commit is successful, return normally. Otherwise execution resumes
at (hw-transaction-begin) by returning abort reason."
    `(sb-transaction:transaction-end))

  (defmacro hw-transaction-rerun-may-succeed? (err-code)
    "If ERR-CODE is the result returned by (HW-TRANSACTION-BEGIN) of an *aborted* transaction,
return T if re-running the same transaction has a possibility to succeed,
i.e. if the abort reason was temporary (as for example a conflict with another thread).
Return NIL if re-running the same transaction has no possibility to succeed."
    `(sb-transaction:transaction-rerun-may-succeed-p ,err-code))


  ;; evaluate (hw-transaction-supported?) only once: CPUID is expensive,
  ;; flushes all caches and aborts hardware transactions.
  (defconstant +hw-transaction-supported+ (hw-transaction-supported?)))




#?-hw-transactions
(progn

  ;; stub implementation if no compiler support

  (defconstant +hw-transaction-started+ 0)

  (defmacro hw-transaction-supported? ()
    "Return T if the CPU supports hardware memory transactions,
and there is a compiler extension to use them,
otherwise return NIL."
    `nil)

  (defmacro hw-transaction-begin ()
    "Start a hardware memory transaction. Return +transaction-started+
if transaction started successfully, otherwise return abort reason."
    `-1)

  (defmacro hw-transaction-running? ()
    "Return T if a hardware memory transaction is in progress."
    `nil)

  (defmacro hw-transaction-abort ()
    "Abort a hardware memory transaction currently in progress.
Causes a rollback of *all* transaction effects, execution resumes
at (hw-transaction-begin) by returning abort reason."
    `(error "hardware memory transaction not supported"))

  (defmacro hw-transaction-end ()
    "Try to commit a hardware memory transaction currently in progress.
If commit is successful, return normally. Otherwise execution resumes
at (hw-transaction-begin) by returning abort reason."
    `nil)

  (defmacro hw-transaction-rerun-may-succeed? (err-code)
    "If ERR-CODE is the result returned by (HW-TRANSACTION-BEGIN) of an *aborted* transaction,
return T if re-running the same transaction has a possibility to succeed,
i.e. if the abort reason was temporary (as for example a conflict with another thread).
Return NIL if re-running the same transaction has no possibility to succeed."
    `nil)


  (defconstant +hw-transaction-supported+ nil))






(defmacro hw-transaction-supported-and-running? ()
  `(and +hw-transaction-supported+ (hw-transaction-running?)))


;; avoid "unexpected EOF" compiler error
;; if none of the conditional compilations above are satisfied
nil
