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

;;;; ** Transactional variables

(defun tvar (&optional (value +unbound-tvar+))
  (the tvar
    (make-tvar :value value :id (tvar-id/next))))

(declaim (ftype (function (#-ecl tvar #+ecl t) t) $)
         (ftype (function (t tvar) t) (setf $)))
         

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

  
;;;; ** Reading and writing



(declaim (ftype (function (t tvar) t)           (setf raw-value-of))
         (inline
           (setf raw-value-of)))

(defun (setf raw-value-of) (value var)
  "Set the VALUE of VAR. Return VALUE.
This function intentionally ignores transactions and it is only useful
for debugging purposes. Use (setf ($ var) value) instead."
  (declare (type tvar var))
  (set-tvar-value-and-version var value
                              (global-clock/sw-write
                               (global-clock/start-write
                                (global-clock/start-read)))))

  
  


(defun tx-read-of (var &optional (log (current-tlog)))
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
(defun tx-write-of (var value &optional (log (current-tlog)))
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (set-txhash (tlog-writes log) var value))




(declaim (inline $-tx))
(defun $-tx (var)
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY inside software memory transactions."
  (declare (type tvar var))
  (tx-read-of var))


(declaim (inline (setf $-tx)))
(defun (setf $-tx) (value var)
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY inside software memory transactions."
  (declare (type tvar var))
  (tx-write-of var value))





(declaim (inline $-hwtx))
#?+hw-transactions
(defun $-hwtx (var)
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY inside hardware memory transactions."
  (declare (type tvar var))
  (tvar-value var))


(declaim (inline (setf $-hwtx)))
#?+hw-transactions
(defun (setf $-hwtx) (value var &optional (version (hw-tlog-write-version)))
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY inside hardware memory transactions."
  (declare (type tvar var))
  (setf (tvar-value var) value
        (tvar-version var) version))




(declaim (inline $-notx))
(defun $-notx (var)
    "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.
Works ONLY outside memory transactions."
  (declare (type tvar var))

  ;; No transaction. Return raw value for debugging purposes,
  ;; ignoring any lock or inconsistency
  (raw-value-of var))


(declaim (inline (setf $-notx)))
(defun (setf $-notx) (value var)
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY outside memory transactions."
  (declare (type tvar var))

  (setf (raw-value-of var) value))



(defmacro use-$-tx? ()
  "Return T if $-tx and (setf $-tx) should be used, otherwise return NIL."
  `(recording?))


(declaim (inline $-noerror))
(defun $-noerror (var)
  "Get the value from the transactional variable VAR and return it.
Return +unbound-tvar+ if VAR is not bound to a value.

Works both inside and outside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."

  (if t ;;(use-$-hwtx?) ($-hwtx var)
      (if (use-$-tx?) ($-tx var)
          ($-notx var))))

               
(defun $ (var)
  "Get the value from the transactional variable VAR and return it.
Signal an error if VAR is not bound to a value.

Works both inside and outside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value ($-noerror var)
    (unless (eq value +unbound-tvar+)
      (return-from $ value))
    (unbound-tvar-error var)))



(defun (setf $) (value var)
  "Store VALUE inside transactional variable VAR and return VALUE.

Works both outside and inside transactions.
During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  (if t ;;(use-$-hwtx?) (setf ($-hwtx var) value)
      (if (use-$-tx?) (setf ($-tx var) value)
          (setf ($-notx var) value))))



  
(defun bound-$? (var)
    "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (not (eq +unbound-tvar+ ($-noerror var))))


(defun unbind-$ (var)
    "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
  (declare (type tvar var))

  (setf ($ var) +unbound-tvar+)
  var)


(declaim (inline try-take-$ try-put-$))

(defun peek-$ (var &optional default)
    "Get the value from the transactional variable VAR
and return it and t as multiple values.
If VAR is not bound to a value, return (values DEFAULT nil).

Works both inside and outside transactions."
  (declare (type tvar var))

  (let1 value ($-noerror var)
    (if (eq value +unbound-tvar+)
      (values default nil)
      (values value   t))))


(defun try-take-$ (var &optional default)
    "Get the value from the transactional variable VAR,
unbind it and and return t and the original value as multiple values.
If VAR is not bound to a value, return (values nil DEFAULT).

Works only inside software transactions."
  (declare (type tvar var))

  (let1 value ($-tx var)
    (if (eq value +unbound-tvar+)
        (values nil default)
        (progn
          (setf ($-tx var) +unbound-tvar+)
          (values t value)))))


(defun try-put-$ (var value &optional default)
    "If VAR is not bound, bind it to VALUE and return (values VALUE t)
If VAR is already bound to a value, return (values DEFAULT nil).

Works only inside software transactions."
  (declare (type tvar var))

  (let1 old-value ($-tx var)
    (if (eq old-value +unbound-tvar+)
        (values t (setf ($-tx var) value))
        (values nil default))))



;;;; ** Accessors


(defmethod value-of ((var tvar))
  "Return the value inside a TVAR. Works both outside and inside transactions.
Equivalent to ($ var)"
  ($ var))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a TVAR. Works both outside and inside transactions.
Equivalent to (setf ($ var) value)"
  (setf ($ var) value))



;;;; ** Locking and unlocking

(declaim (ftype (function (tvar t tlog) boolean) tvar-valid-and-unlocked?
                #||#                             tvar-valid-and-own-or-unlocked?)
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

    #?+(eql tvar-lock :none)
    t

    #?-(eql tvar-lock :none)
    (progn
      #?+(and mutex-owner (eql tvar-lock :mutex))
      (mutex-is-own? (the mutex var))

      #?-(and mutex-owner (eql tvar-lock :mutex))
      ;; check transaction log to detect if we're the ones that locked VAR.
      ;; this may be needed both when TVAR-LOCK feature is :BIT
      ;; and when it is :MUTEX but we have no MUTEX-OWNER implementation
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


;;;; ** Printing

(defprint-object (var tvar :identity nil)

  (let ((value +unbound-tvar+)
        (present? nil)
        (id (~ var)))

    (when (recording?)
      ;; extract transactional value without triggering a (rerun):
      ;; printing TVARs is _only_ for debugging purposes
      (multiple-value-bind (tx-value tx-present?)
          (get-txhash (tlog-writes (current-tlog)) var)
        (when tx-present?
          (setf value    tx-value
                present? tx-present?))))

    (unless present?
      (setf value (tvar-value var)))

    (if (eq value +unbound-tvar+)
        (format t "~A unbound" id)
        (format t "~A [~A]" id value))))
