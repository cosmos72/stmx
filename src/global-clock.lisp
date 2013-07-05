;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013 massimiliano ghilardi
;;
;; this library is free software: you can redistribute it and/or
;; modify it under the terms of the lisp lesser general public license
;; (http://opensource.franz.com/preamble.html), known as the llgpl.
;;
;; this library is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.
;; see the lisp lesser general public license for more details.


(in-package :stmx)


(enable-#?-syntax)

;;;; ** global clock - exact, atomic counter for transaction validation:
;;;; used to ensure transactional read consistency.


(deftype version-type () 'atomic-counter-num)

(defconstant +global-clock-delta+ 2
  "*tlog-counter* is incremented by 2 each time: the lowest bit is used
as \"locked\" flag in TVARs versioning.")


(declaim (type atomic-counter +global-clock+))
(eval-always
 (defvar     *global-clock* (make-atomic-counter)))
(defconstant +global-clock+ *global-clock*)

(declaim (ftype (function () version-type) global-clock/on-read)
         (ftype (function (version-type version-type) boolean) global-clock/valid-read?)
         
         (ftype (function (version-type) version-type) global-clock/on-write
                #||#                                   global-clock/on-abort)

         (inline global-clock/on-read global-clock/valid-read?
                 global-clock/on-write global-clock/on-abort))

#-always ;; #?+global-clock-gv1
(progn
  (defun global-clock/on-read ()
    "Return the value to use as transaction \"read version\".

This function must be invoked once upon starting a transaction for the first time.
In case the transaction just aborted and is being re-executed, invoke instead
\(GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION).

This is GV1 implementation - it returns the current +global-clock+ value."
    (get-atomic-counter +global-clock+))


  (defun global-clock/valid-read? (tvar-version read-version)
    "Return T if TVAR-VERSION is compatible with transaction \"read version\".
If this function returns NIL, the transaction must be aborted.

During software transactions, this function must be invoked after every
TVAR read and before returning the TVAR value to the application code.
During hardware transactions, this function is not used.

This is GV1 implementation - it returns (<= tvar-version read-version)"
    (<= tvar-version read-version))


  (defun global-clock/on-write (read-version)
    "Return the value to use as transaction \"write version\", given the transaction
current READ-VERSION that was assigned at transaction start.

During software-base commits, this function must be called once before
the first TVAR write; while during hardware-based commits and transactions
it must be called once before writing the first TVAR.

This is GV1 implementation - it atomically increments +global-clock+
and returns the new value."
    (declare (ignore read-version))

    (incf-atomic-counter +global-clock+ +global-clock-delta+))


  (defun global-clock/on-abort (read-version)
    "Return the value to use as new transaction \"read version\",
given the current transaction READ-VERSION that was set at transaction start
either by (GLOBAL-CLOCK/ON-READ) or by (GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION).

This function must be called before rerunning a transaction that failed/aborted.

This is GV1 implementation - it returns the current +global-clock+ value."
    (declare (ignore read-version))

    (get-atomic-counter +global-clock+)))










#?+global-clock-gv5
(progn
  (defun global-clock/on-read ()
    "Return the value to use as transaction \"read version\".

This function must be invoked once upon starting a transaction for the first time.
In case the transaction just aborted and is being re-executed, invoke instead
\(GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION).

This is GV5 implementation - it returns the current +global-clock+ value."
    (get-atomic-counter +global-clock+))


  (defun global-clock/valid-read? (tvar-version read-version)
    "Return T if TVAR-VERSION is compatible with transaction \"read version\".
If this function returns NIL, the transaction must be aborted.

During software transactions, this function must be invoked after every
TVAR read and before returning the TVAR value to the application code.
During hardware transactions, this function is not used.

This is GV1 implementation - it returns (<= tvar-version read-version)"
    (<= tvar-version read-version))


  (defun global-clock/on-write (read-version)
    "Return the value to use as the transaction \"write version\", given the transaction
current READ-VERSION that was assigned at transaction start.

During software-base commits, this function must be called once before
the first TVAR write; while during hardware-based commits and transactions
it must be called once before writing the first TVAR.

This is GV5 implementation - returns (1+ +global-clock+) without incrementing it."
    (declare (ignore read-version))

    (get-atomic-counter-plus-delta +global-clock+ +global-clock-delta+))

  (defun global-clock/on-abort (read-version)
    "Return the value to use as new transaction \"read version\",
given the current transaction READ-VERSION that was set at transaction start
either by (GLOBAL-CLOCK/ON-READ) or by (GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION).

This function must be called before rerunning a transaction that failed/aborted.

This is GV1 implementation - it returns the current +global-clock+ value."
    (declare (ignore read-version))

    (incf-atomic-counter +global-clock+ +global-clock-delta+)))
