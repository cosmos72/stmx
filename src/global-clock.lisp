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
(define-constant-eval-once +global-clock-incrementable+ (make-atomic-counter))

(eval-always
  (let1 stmx-package (find-package 'stmx)
    (defun %gvx-expand0-f (prefix suffix)
      (declare (type symbol prefix suffix))
      (intern (concatenate 'string
                           (symbol-name prefix)
                           "/"
                           (symbol-name suffix))
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








;;;; ** This is global-clock version GV1

(deftype gv1/version-type () 'global-clock-incrementable/version-type)

(define-symbol-macro +gv1+   +global-clock-incrementable+)


(defmacro gv1/features ()
  "This is GV1 implementation of GLOBAL-CLOCK/FEATURES.

Return nil, i.e. not '(:suitable-for-hw-transactions) because
\(GV1/START-WRITE ...) increments the global clock, which causes conflicts
and aborts when multiple hardware transactions are running simultaneously."
  'nil)


(defmacro gv1/start-read ()
  "This is GV1 implementation of GLOBAL-CLOCK/START-READ.
Return the current +gv1+ value."
  `(get-atomic-counter +gv1+))


(defmacro gv1/valid-read? (tvar-version read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/VALID-READ?
Return (<= tvar-version read-version)"
  `(<= (the gv1/version-type ,tvar-version) (the gv1/version-type ,read-version)))


(defmacro gv1/start-write (read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/START-WRITE.
Atomically increment +gv1+ and return its new value."
  (declare (ignore read-version))

  `(incf-atomic-counter +gv1+ +global-clock-delta+))


(defmacro gv1/sw-write (write-version)
  "This is GV1 implementation of GLOBAL-CLOCK/SW-WRITE.
Return WRITE-VERSION."
  write-version)


(defmacro gv1/hw-write (write-version)
  "This is GV1 implementation of GLOBAL-CLOCK/HW-WRITE.
Return WRITE-VERSION."
  write-version)


(defmacro gv1/after-abort (read-version)
  "This is GV1 implementation of GLOBAL-CLOCK/AFTER-ABORT.
Return the current +gv1+ value."
  (declare (ignore read-version))

  `(get-atomic-counter +gv1+))









;;;; ** This is global-clock version GV5

(deftype gv5/version-type () 'global-clock-incrementable/version-type)

(define-symbol-macro +gv5+   +global-clock-incrementable+)


(defmacro gv5/features ()
  "This is GV5 implementation of GLOBAL-CLOCK/FEATURES.

Return '(:SUITABLE-FOR-HW-TRANSACTIONS :SPURIOUS-FAILURES-IN-SINGLE-THREAD)
because the global clock is incremented only by GV5/AFTER-ABORT, which avoids
incrementing it in GV5/START-WRITE (it would cause hardware transactions
to conflict with each other and abort) but also causes a 50% abort rate (!) even
in a single, isolated thread reading and writing its own transactional memory."

 ''(:suitable-for-hw-transactions :spurious-failures-in-single-thread))


(defmacro gv5/start-read ()
  "This is GV5 implementation of GLOBAL-CLOCK/START-READ.
Return the current +gv5+ value."
  `(get-atomic-counter +gv5+))


(defmacro gv5/valid-read? (tvar-version read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/VALID-READ?
Return (<= tvar-version read-version)"
  `(<= (the gv5/version-type ,tvar-version) (the gv5/version-type ,read-version)))


(defmacro gv5/start-write (read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/START-WRITE.
Return (1+ +gv5+) without incrementing it."
  (declare (ignore read-version))

  `(get-atomic-counter-plus-delta +gv5+ +global-clock-delta+))


(defmacro gv5/sw-write (write-version)
  "This is GV5 implementation of GLOBAL-CLOCK/SW-WRITE.
Return (1+ +gv5+) without incrementing it."
  (declare (ignore write-version))

  `(get-atomic-counter-plus-delta +gv5+ +global-clock-delta+))


(defmacro gv5/hw-write (write-version)
  "This is GV5 implementation of GLOBAL-CLOCK/HW-WRITE.
Return WRITE-VERSION."
  write-version)


(defmacro gv5/after-abort (read-version)
  "This is GV5 implementation of GLOBAL-CLOCK/AFTER-ABORT.
Increment +gv5+ and return its new value."
  (declare (ignore read-version))
  `(incf-atomic-counter +gv5+ +global-clock-delta+))







;;;; ** choose which global-clock implementation to use


(deftype global-clock/version-type () (%gv-expand0-f 'version-type))
(deftype              version-type () (%gv-expand0-f 'version-type))


(defmacro global-clock/features ()
  "Return the features of the GLOBAL-CLOCK algorithm, i.e. a list
containing zero or more of :SUITABLE-FOR-HW-TRANSACTIONS and
:SPURIOUS-FAILURES-IN-SINGLE-THREAD. The list of possible features
will be expanded as more GLOBAL-CLOCK algorithms are implemented."
  `(%gv-expand features))


(defmacro global-clock/start-read ()
  "Return the value to use as transaction \"read version\".

This function must be invoked once upon starting a transaction for the first time.
In case the transaction just aborted and is being re-executed, invoke instead
\(GLOBAL-CLOCK/AFTER-ABORT PREVIOUS-READ-VERSION)."
  `(%gv-expand start-read))


(defmacro global-clock/valid-read? (tvar-version read-version)
  "Return T if TVAR-VERSION is compatible with transaction \"read version\".
If this function returns NIL, the transaction must be aborted.

During software transactions, this function must be invoked after every
TVAR read and before returning the TVAR value to the application code.
During hardware transactions, this function is not used."
  `(%gv-expand valid-read? ,tvar-version ,read-version))


(defmacro global-clock/start-write (read-version)
  "Return the value to use as transaction \"write version\", given the transaction
current READ-VERSION that was assigned at transaction start.

During software-base commits, this function must be called once before
the first TVAR write; while during hardware-based commits and transactions
it must be called once before writing the first TVAR."
  `(%gv-expand start-write ,read-version))


(defmacro global-clock/sw-write (write-version)
  "Return the value to use as TVAR \"write version\", given the transaction
current WRITE-VERSION that was assigned by GLOBAL-CLOCK/START-WRITE before
the transaction started writing to TVARs.

Fhis function must be called for **each** TVAR being written
during software-based commit phase of transactions."
  `(%gv-expand sw-write ,write-version))


(defmacro global-clock/hw-write (write-version)
  "Return the value to use as TVAR \"write version\", given the transaction
current WRITE-VERSION that was assigned by GLOBAL-CLOCK/START-WRITE before
the transaction started writing to TVARs.

Fhis function must be called for **each** TVAR being written during
hardware-assisted commit phase of software transactions
and during pure hardware transactions."
  `(%gv-expand hw-write ,write-version))


(defmacro global-clock/after-abort (read-version)
  "Return the value to use as new transaction \"read version\",
given the current transaction READ-VERSION that was set at transaction start
either by (GLOBAL-CLOCK/START-READ) or by (GLOBAL-CLOCK/AFTER-ABORT PREVIOUS-READ-VERSION).

This function must be called after a transaction failed/aborted and before rerunning it."
  `(%gv-expand after-abort ,read-version))


(eval-always
  (defun global-clock/publish-features ()
    "Publish (GLOBAL-CLOCK/FEATURES) to stmx.lang::*feature-list*
so they can be tested with #?+ and #?- reader macros."
    (loop for pair in (global-clock/features) do
         (let* ((feature (if (consp pair) (first pair) pair))
                (value   (if (consp pair) (rest  pair) t))
                (gv-feature (%gvx-expand0-f 'global-clock feature)))

           (stmx.lang::add-feature gv-feature value))))

  (global-clock/publish-features))
     

      