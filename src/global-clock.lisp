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


(defconstant +global-clock-delta+ 2
  "+global-clock+ is incremented by 2 each time: the lowest bit is reserved
as \"locked\" flag in TVARs versioning (it is actually used only if
#?+fast-lock feature is present).")


;;;; ** definitions common to more than one global-clock implementation

(deftype global-clock-incrementable/version-type () 'atomic-counter-num)

(declaim (type atomic-counter +global-clock-incrementable+))
(defconstant-eval-once +global-clock-incrementable+ (make-atomic-counter))

(eval-always
  (let1 stmx-package (find-package 'stmx)
    (defun %gvx-expand0-f (gvx name)
      (declare (type symbol gvx name))
      (intern (concatenate 'string
                           (symbol-name gvx)
                           "/"
                           (symbol-name name))
              stmx-package)))

  (defun %gv-expand0-f (name)
    (declare (type symbol name))
    (%gvx-expand0-f (stmx.lang::get-feature 'global-clock) name)))


(defmacro %gvx-expand (gvx name &rest args)
  (declare (type symbol gvx name))
  (let ((full-name (%gvx-expand0-f gvx name)))
    `(,full-name ,@args)))

(defmacro %gv-expand (name &rest args)
  (declare (type symbol name))
  (let ((full-name (%gv-expand0-f name)))
    `(,full-name ,@args)))





(defmacro declaim-global-clock-functions (gvx)
  (let ((version-type (%gvx-expand0-f gvx 'version-type))
        (on-read      (%gvx-expand0-f gvx 'on-read))
        (valid-read?  (%gvx-expand0-f gvx 'valid-read?))
        (on-write     (%gvx-expand0-f gvx 'on-write))
        (on-abort     (%gvx-expand0-f gvx 'on-abort)))
        
    `(declaim
      (ftype (function () ,version-type) ,on-read)

      (ftype (function (,version-type ,version-type) boolean) ,valid-read?)
         
      (ftype (function (,version-type) ,version-type) ,on-write ,on-abort)

      (inline ,on-read ,valid-read? ,on-write ,on-abort))))








;;;; ** This is global-clock version GV1

(deftype gv1/version-type () 'global-clock-incrementable/version-type)

(define-symbol-macro +gv1+   +global-clock-incrementable+)

;;(declaim-global-clock-functions gv1)

(defmacro gv1/on-read ()
  "This is GV1 implementation of GLOBAL-CLOCK/ON-READ.
Return the current +gv1+ value."
  `(get-atomic-counter +gv1+))


(defmacro gv1/valid-read? (tvar-version read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/VALID-READ?
Return (<= tvar-version read-version)"
  `(<= (the gv1/version-type ,tvar-version) (the gv1/version-type ,read-version)))


(defmacro gv1/on-write (read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/ON-WRITE.
Atomically increment +gv1+ and return its new value."
  (declare (ignore read-version))

  `(incf-atomic-counter +gv1+ +global-clock-delta+))


(defmacro gv1/on-abort (read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/ON-ABORT.
Return the current +gv1+ value."
  (declare (ignore read-version))

  `(get-atomic-counter +gv1+))








;;;; ** This is global-clock version GV5

(deftype gv5/version-type () 'global-clock-incrementable/version-type)

(define-symbol-macro +gv5+   +global-clock-incrementable+)

;;(declaim-global-clock-functions gv5)

(defmacro gv5/on-read ()
  "This is GV5 implementation of GLOBAL-CLOCK/ON-READ.
Return the current +gv5+ value."
  `(get-atomic-counter +gv5+))


(defmacro gv5/valid-read? (tvar-version read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/VALID-READ?
Return (<= tvar-version read-version)"
  `(<= (the gv5/version-type ,tvar-version) (the gv5/version-type ,read-version)))


(defmacro gv5/on-write (read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/ON-WRITE?
Return (1+ +gv5+) without incrementing it."
  (declare (ignore read-version))

  `(get-atomic-counter-plus-delta +gv5+ +global-clock-delta+))

(defmacro gv5/on-abort (read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/ON-ABORT?
Return the current +gv5+ value."
  (declare (ignore read-version))

  `(incf-atomic-counter +gv5+ +global-clock-delta+))







;;;; ** choose which global-clock implementation to use


(deftype global-clock/version-type () (%gv-expand0-f 'version-type))
(deftype              version-type () (%gv-expand0-f 'version-type))

;; (declaim-global-clock-functions global-clock)

(defmacro global-clock/on-read ()
  "Return the value to use as transaction \"read version\".

This function must be invoked once upon starting a transaction for the first time.
In case the transaction just aborted and is being re-executed, invoke instead
\(GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION)."
  `(%gv-expand on-read))


(defmacro global-clock/valid-read? (tvar-version read-version)
  "Return T if TVAR-VERSION is compatible with transaction \"read version\".
If this function returns NIL, the transaction must be aborted.

During software transactions, this function must be invoked after every
TVAR read and before returning the TVAR value to the application code.
During hardware transactions, this function is not used."
  `(%gv-expand valid-read? ,tvar-version ,read-version))


(defmacro global-clock/on-write (read-version)
  "Return the value to use as transaction \"write version\", given the transaction
current READ-VERSION that was assigned at transaction start.

During software-base commits, this function must be called once before
the first TVAR write; while during hardware-based commits and transactions
it must be called once before writing the first TVAR."
  `(%gv-expand on-write read-version))


(defmacro global-clock/on-abort (read-version)
  "Return the value to use as new transaction \"read version\",
given the current transaction READ-VERSION that was set at transaction start
either by (GLOBAL-CLOCK/ON-READ) or by (GLOBAL-CLOCK/ON-ABORT PREVIOUS-READ-VERSION).

This function must be called before rerunning a transaction that failed/aborted."
  `(%gv-expand on-abort read-version))
