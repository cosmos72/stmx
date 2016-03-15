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

;;;; ** Transactional variables

(defun tvar (&optional (value +unbound-tvar+))
  (the tvar
    (make-tvar :value value :id (tvar-id/next))))


;;;; ** Reading and writing


(declaim (ftype (function (t tvar) (values t &optional)) (setf raw-value-of))
         (notinline (setf raw-value-of)))

(defun (setf raw-value-of) (value var)
  "Set the VALUE of VAR. Return VALUE.
This function intentionally ignores transactions and it is only useful
for debugging purposes. Use (SETF ($-SLOT VAR) VALUE) or (SETF ($ VAR) VALUE) instead."
  (declare (type tvar var))
  (set-tvar-value-and-version var value
                              (global-clock/sw/write
                               (global-clock/sw/start-write
                                (global-clock/sw/start-read)))))

  
  
(defun tx-read-of (var log)
  "If VAR's value is stored in writes of transaction LOG, return the stored value.
Otherwise, validate VAR version, add VAR into the reads of transaction LOG,
and return (raw-value-of VAR). If VAR version validation fails, call (rerun).

TX-READ-OF is an internal function called by ($ VAR) and by reading TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (multiple-value-bind (value present?)
      (get-txhash (tlog-writes log) var)
    (when present?
      (return-from tx-read-of value)))

  (multiple-value-bind (value version fail) (tvar-value-and-version-or-fail var)

    (declare (type atomic-counter-num version)
             (type bit fail))

    ;; TVAR is locked or could not be read?
    (unless (and (eql 0 fail)
                 ;; TVAR may have changed after this TX started?
                 (global-clock/valid-read? version (tlog-read-version log)))
      ;; inconsistent data
      (rerun))

    (set-txhash (tlog-reads log) var value)))


(declaim (inline tx-write-of))
(defun tx-write-of (value var log)
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (SETF ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (set-txhash (tlog-writes log) var value))



;;;; ** hwtx

#?+hw-transactions
(progn
  (declaim (inline $-hwtx))
  (defun $-hwtx (var &optional (hwtx-version (hw-tlog-write-version)))
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY inside hardware memory transactions.

HWTX-VERSION is not used, its only purpose is symmetry with (setf $-hwtx)"
    (declare (ignore hwtx-version)
             (type tvar var))
    (tvar-value var))


  ;; WITH-TX uses macrolet, which cannot bind names like (setf ...)
  ;; thus we use define set-$-hwtx and create a setf-expander on $-hwtx

  (declaim (inline set-$-hwtx))
  (defun set-$-hwtx (var value &optional (hwtx-version (hw-tlog-write-version)))
    "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY inside hardware memory transactions.

HWTX-VERSION *must* be equal to the value returned by (HW-TLOG-WRITE-VERSION),
it is a per-transaction constant.
Its purpose is to speed up this function, by removing the two nanoseconds
required to retrieve such value from thread-local storage."
    (declare (type tvar var)
             (type version-type hwtx-version))
    (setf (tvar-version var) hwtx-version
          (tvar-value var) value))

  ;; support live upgrade, older versions had (defun (setf $-hwtx))
  (fmakunbound '(setf $-hwtx))

  (defsetf $-hwtx (var &optional (hwtx-version '(hw-tlog-write-version))) (value)
    `(set-$-hwtx ,var ,value ,hwtx-version)))



;;;; ** swtx

(declaim (inline $-swtx))
(defun $-swtx (var &optional (log (current-tlog)))
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY inside software memory transactions."
  (declare (type tvar var))
  (tx-read-of var log))


;; WITH-TX uses macrolet, which cannot bind names like (setf ...)
;; thus we use define set-$-swtx and create a setf-expander on $-swtx
(declaim (inline set-$-swtx))
(defun set-$-swtx (var value &optional (log (current-tlog)))
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY inside software memory transactions."
  (declare (type tvar var)
           (type tlog log))
  (tx-write-of value var log))

;; support live upgrade, older versions had (defun (setf $-hwtx))
(fmakunbound '(setf $-swtx))

(defsetf $-swtx (var &optional (log '(current-tlog))) (value)
  `(set-$-swtx ,var ,value ,log))





;;;; ** notx

(declaim (inline $-notx))
(defun $-notx (var)
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY outside memory transactions."
  (declare (type tvar var))

  ;; No transaction. Return raw value for debugging purposes,
  ;; ignoring any lock or inconsistency
  (raw-value-of var))


;; WITH-TX uses macrolet, which cannot bind names like (setf ...)
;; thus we use define set-$-hwtx and create a setf-expander on $-hwtx
(declaim (inline set-$-notx))
(defun set-$-notx (var value)
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY outside memory transactions."
  (declare (type tvar var))

  (setf (raw-value-of var) value))

;; support live upgrade, older versions had (defun (setf $-notx))
(fmakunbound '(setf $-notx))

(defsetf $-notx set-$-notx)





;;; ** more macros

(defmacro use-$-hwtx? ()
  "Return T if $-hwtx and (setf $-hwtx) should be used, otherwise return NIL."
  #?+hw-transactions
  `(/= +invalid-version+ *hw-tlog-write-version*)
  #?-hw-transactions
  `nil)


(defmacro use-$-swtx? ()
  "Return T if $-swtx and (setf $-swtx) should be used, otherwise return NIL."
  `(recording?))


;; finally, the two core functions to read/write TVARs:
;; ($ TVAR) AND (SETF ($ TVAR) VALUE)

(declaim (ftype (function (#-ecl tvar #+ecl t) (values t &optional)) $)
         (ftype (function (tvar t) (values t &optional)) set-$)
         (inline $)
         (notinline set-$))


(defun $ (var)
  "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.

Works both inside and outside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  #?+hw-transactions
  (let ((hwtx-version (hw-tlog-write-version)))
    (unless (= +invalid-version+ hwtx-version)
      (return-from $ ($-hwtx var hwtx-version))))

  (if (use-$-swtx?)
      ($-swtx var)
      ($-notx var)))

               
;; WITH-TX uses macrolet, which cannot bind names like (setf ...)
;; thus we use define set-$ and create a setf-expander on $
(defun set-$ (var value)
  "Store VALUE inside transactional variable VAR and return VALUE.

Works both outside and inside transactions.
During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  #?+hw-transactions
  (let ((hwtx-version (hw-tlog-write-version)))
    (when (/= +invalid-version+ hwtx-version)
      (return-from set-$ (set-$-hwtx var value hwtx-version))))

  (if (use-$-swtx?)
      (set-$-swtx var value)
      (set-$-notx var value)))

;; support live upgrade, older versions had (defun (setf $))
(fmakunbound '(setf $))

(defsetf $ set-$)

  


;;;; ** Locking and unlocking

(declaim (ftype (function (tvar t tlog) (values boolean &optional))
                tvar-valid-and-unlocked?
                tvar-valid-and-own-or-unlocked?)
         (inline
           tvar-valid-and-unlocked?
           tvar-valid-and-own-or-unlocked?))


(defun tvar-valid-and-unlocked? (var expected-value log)
  "Return T if VAR is valid, i.e. its value is EQ to EXPECTED-VAL
and VAR is unlocked.
Return NIL if VAR has different value, or is locked by some thread (including current thread)."
  (declare (type tvar var)
           (type tlog log)
           (ignore log))
  
  (multiple-value-bind (value version fail) (tvar-value-and-version-or-fail var)
    (declare (ignore version)
             (type bit fail))

    (and (eq value expected-value)
         (eql fail 0))))


(defun tvar-valid-and-own-or-unlocked? (var expected-value log)
  "Return T if VAR is valid, i.e. its value is EQ to EXPECTED-VAL
and VAR is locked by current thread or unlocked.
Return NIL if VAR has different value, or is locked by some other thread."
  (declare (type tvar var)
           (type tlog log)
           (ignorable log))
    

  (multiple-value-bind (value version fail) (tvar-value-and-version-or-fail var)
    (declare (ignore version)
             (type bit fail))

    (unless (eq value expected-value)
      (return-from tvar-valid-and-own-or-unlocked? nil))

    (when (eql fail 0)
      (return-from tvar-valid-and-own-or-unlocked? t))

    (progn
      #?+(and mutex-owner (eql tvar-lock :mutex))
      (mutex-is-own? (the mutex var))

      #?-(and mutex-owner (eql tvar-lock :mutex))
      ;; check transaction log to detect if we're the ones that locked VAR.
      ;; this may be needed both when TVAR-LOCK feature is :BIT
      ;; and when TVAR-LOCK is :MUTEX but we have no MUTEX-OWNER implementation
      (let1 present? (nth-value 1 (get-txhash (tlog-writes log) var))
        present?))))


;;;; ** Listening and notifying

(defun listen-tvar (var log)
  "Add LOG to VAR's waiting list, so that LOG will be notified if VAR changes."

  (declare (type tvar var)
           (type tlog log))
  (with-lock ((tvar-waiting-lock var))
    (let1 waiting (tvar-waiting-for var)
      (unless waiting
        (setf waiting (make-hash-table :test 'eq)
              (tvar-waiting-for var) waiting))
      (setf (gethash log waiting) t))))


(defun unlisten-tvar (var log)
  "Remove LOG from VAR's waiting list, so that LOG will *not* be notified anymore
if VAR changes."

  (declare (type tvar var)
           (type tlog log))
  (awhen (tvar-waiting-for var)
    (with-lock ((tvar-waiting-lock var))
      (remhash log it))))


(defun notify-tvar (var)
  "Wake up all threads waiting for VAR to change."
  (declare (type tvar var))
  (with-lock ((tvar-waiting-lock var))
    (awhen (tvar-waiting-for var)
      (do-hash (log) it
        (notify-tlog log var)))))


(defun notify-tvar-high-load (var)
  "Wake up all threads waiting for VAR to change."
  (declare (type tvar var))
  ;; this is (almost) the infamous double-check locking anti-pattern.
  ;; in this particular case, it is safe to check the lazily-initialized
  ;; slot (tvar-waiting-for var) without locking, 
  ;; because we actually use it only while holding the lock.
  ;;
  ;; the effect of this trick is a significant speedup under heavy load
  ;; and under light load (i.e. single-thread), at the price of increasing
  ;; the conflict rate and SLIGHTLY slowing down under moderate load
  (awhen (tvar-waiting-for var)
    (with-lock ((tvar-waiting-lock var))
      (do-hash (log) it
        (notify-tlog log var)))))


;;;; ** Signalling unbound variables

(defun unbound-tvar-error (var)
  "Signal an unbound-slot error and allow the user to continue by specifying or storing a value."
  (declare (type tvar var))
  (restart-case (error 'unbound-slot :instance var :name 'value)
    (use-value (value)
      :report "Specify a value to use."
      :interactive (lambda ()
                     (format t "~&Value to use: ")
                     (list (eval (read))))
      value)
    (store-value (value)
      :report "Specify a value to use and store."
      :interactive (lambda ()
                     (format t "~&Value to use and store: ")
                     (list (eval (read))))
      (setf ($ var) value)
      value)))

  
;;;; ** Printing

(defprint-object (var tvar :identity nil)

  (let ((value (tvar-value var))
        (version (tvar-version var))
        (id (~ var))
        (log (current-tlog)))

    (when log
      ;; extract transactional value without triggering a (rerun):
      ;; printing TVARs is _only_ for debugging purposes
      (multiple-value-bind (tx-value tx-present?)
          (get-txhash (tlog-writes log) var)
        (when tx-present?
          (setf value    tx-value
                version (tlog-read-version log)))))

    (if (eq value +unbound-tvar+)
        (format t "unbound v:~A id:~A" version id)
        (format t "[~A] v:~A id:~A" value version id))))
