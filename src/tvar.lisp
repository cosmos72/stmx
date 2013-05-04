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

;;;; ** Transactional variables

(declaim (inline tvar))
(defun tvar (&optional (value +unbound+))
  #+stmx.have-atomic-ops
  (the tvar (make-tvar :value value))
  #-stmx.have-atomic-ops
  (the tvar (make-tvar :versioned-value
                       (if (eq value +unbound+)
                           +versioned-unbound+
                           (cons +invalid-counter+ value)))))

(declaim (ftype (function (tvar) t) $)
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



(declaim (ftype (function (tvar) (values fixnum t)) tvar-version-and-value)
         (ftype (function (tvar fixnum t) t)    set-tvar-version-and-value)
         (ftype (function (t tvar) t)           (setf raw-value-of))
         (inline
           tvar-version-and-value set-tvar-version-and-value (setf raw-value-of)))

(defun tvar-version-and-value (var)
  "return as multiple values:
1) the current version of VAR
2) the current value of VAR, or +unbound+ if VAR is not bound to a value."
  (declare (type tvar var))

  (let*
      #+stmx.have-atomic-ops
    ((version (progn (atomic-read-barrier) (tvar-version var)))
     (value   (progn (atomic-read-barrier) (tvar-value   var))))

    #-stmx.have-atomic-ops
    ((pair (tvar-versioned-value var))
     (version (first pair))
     (value (rest pair)))

    (values version value)))



(defun set-tvar-version-and-value (var version value)
  "Set the VERSION and VALUE of VAR. Return VALUE."
  (declare (type tvar var)
           (type fixnum version))

  #+stmx.have-atomic-ops
  (progn
    (atomic-write-barrier
      (setf (tvar-version var) version))
    (atomic-write-barrier
      (setf (tvar-value   var) value)))
  #-stmx.have-atomic-ops
  (progn
    (setf (tvar-versioned-value var) (cons version value))
    value))


(defun (setf raw-value-of) (value var)
  "Set the VALUE of VAR. Return VALUE."
  (declare (type tvar var))
  (set-tvar-version-and-value var
                              (incf-atomic-counter *tlog-counter*)
                              value))
           

  
  


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

  (multiple-value-bind (version value) (tvar-version-and-value var)

    (when (> (the fixnum version) (tlog-id log))
      ;; inconsistent data? let's check
      (multiple-value-bind (prev-value present?)
          (get-txhash (tlog-reads log) var)
        (when (and present? (eq value prev-value))
          (return-from tx-read-of value)))
      ;; yes, inconsistent data
      (rerun))

    (set-txhash (tlog-reads log) var value)))



(defun tx-write-of (var value &optional (log (current-tlog)))
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (set-txhash (tlog-writes log) var value))



(defun $ (var)
    "Get the value from the transactional variable VAR and return it.
Signal an error if VAR is not bound to a value.

Works both outside and inside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value (if (recording?)
                  (tx-read-of var)
                  (raw-value-of var))
    (unless (eq value +unbound+)
      (return-from $ value))
    (unbound-tvar-error var)))



(defun (setf $) (value var)
    "Store VALUE inside transactional variable VAR and return VALUE.

Works both outside and inside transactions.
During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  (if (recording?)
      (tx-write-of var value)
      (setf (raw-value-of var) value)))



(declaim (inline fast-$))
(defun fast-$ (var)
    "Get the value from the transactional variable VAR and return it.
Return +unbound+ if VAR is not bound to a value.
Works ONLY inside transactions."
  (declare (type tvar var))
  (tx-read-of var))


(declaim (inline (setf fast-$)))
(defun (setf fast-$) (value var)
  "Store VALUE inside transactional variable VAR and return VALUE.
Works ONLY inside transactions."
  (declare (type tvar var))
  (tx-write-of var value))




               
  
(defun bound-$? (var)
    "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (not (eq +unbound+
           (if (recording?)
               (tx-read-of var)
               (raw-value-of var)))))


(defun unbind-$ (var)
    "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
  (declare (type tvar var))

  (if (recording?)
      (tx-write-of var +unbound+)
      (setf (raw-value-of var) +unbound+))
  var)



(defun peek-$ (var &optional default)
    "Get the value from the transactional variable VAR
and return it and t as multiple values.
If VAR is not bound to a value, return (values default nil).

Works both outside and inside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value (if (recording?)
                  (tx-read-of var)
                  (raw-value-of var))
    (if (eq value +unbound+)
      (values default nil)
      (values value t))))


(defun try-take-$ (var &optional default)
    "Get the value from the transactional variable VAR,
unbind it and and return the original value and t as multiple values.
If VAR is not bound to a value, return (values DEFAULT nil).

Works both outside and inside transactions.
During transactions, it uses transaction log to record the read and write
and to check for any value stored in the log."
  (declare (type tvar var))

  (if (recording?)
      (let1 value (tx-read-of var)
        (if (eq value +unbound+)
            (values default nil)
            (progn
              (tx-write-of var +unbound+)
              (values value t))))
      (let1 value (raw-value-of var)
        (if (eq value +unbound+)
            (values default nil)
            (progn
              (setf (raw-value-of var) +unbound+)
              (values value t))))))


(defun try-put-$ (var value &optional default)
    "If VAR is not bound, bind it to VALUE and return (values VALUE t)
If VAR is already bound to a value, return (values DEFAULT nil).

Works both outside and inside transactions.
During transactions, it uses transaction log to record the read and write
and to check for any value stored in the log."
  (declare (type tvar var))

  (if (recording?)
      (let1 old-value (tx-read-of var)
        (if (eq old-value +unbound+)
            (values (tx-write-of var value) t)
            (values default nil)))
      (let1 old-value (raw-value-of var)
        (if (eq old-value +unbound+)
            (values (setf (raw-value-of var) value) t)
            (values default nil)))))

;;;; ** Accessors


(defmethod value-of ((var tvar))
  "Return the value inside a TVAR. Works both outside and inside transactions."
  ($ var))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a TVAR. Works both outside and inside transactions."
  (setf ($ var) value))



;;;; ** Locking and unlocking

(declaim (ftype (function (tvar)      boolean) try-lock-tvar)
         (ftype (function (tvar)      null)    unlock-tvar)
         (ftype (function (tvar tlog) boolean) tvar-unlocked?)
         (inline
           try-lock-tvar unlock-tvar
           #+stmx.have-atomic-ops tvar-unlocked?))

(defun try-lock-tvar (var)
  "Return T if VAR was locked successfully, otherwise return NIL."
  (stmx.lang::try-acquire-mutex (the mutex var)))
  
  
(defun unlock-tvar (var)
  "Unlock VAR, always return NIL."
  (release-mutex (the mutex var)))


(defun tvar-unlocked? (var log)
  "Return T if VAR is locked by current thread or unlocked.
Return NIL if VAR is locked by some other thread."
  (declare (ignorable log))

  #+stmx.have-atomic-ops
  (mutex-is-own-or-free? (the mutex var))
  #-stmx.have-atomic-ops
  ;; check transaction log writes first
  (multiple-value-bind (value present?)
      (get-txhash (tlog-writes log) var)
    (declare (ignore value))
    (if present?
        t
        ;; then fall back on trying to acquire the lock (messy)
        (let1 lock (mutex-lock (the mutex var))
          (when (bt:acquire-lock lock nil)
            (bt:release-lock lock)
            t)))))
  


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

(defprint-object (obj tvar :identity nil)
  (multiple-value-bind (value present?) (peek-$ obj)
    (if present?
        (format t "~A [~A]" (~ obj) value)
        (format t "~A unbound" (~ obj)))))
