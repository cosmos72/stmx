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
    (unless (eq value +unbound-tvar+)
      (return-from $ value))
    (unbound-tvar-error var)))


(defun (setf $) (value var)
    "Store VALUE inside transactional variable VAR.

Works both outside and inside transactions.
During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  (if (recording?)
      (tx-write-of var value)
      (setf (raw-value-of var) value)))
               
  
(defun bound-$? (var)
    "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (not (eq +unbound-tvar+
           (if (recording?)
               (tx-read-of var)
               (raw-value-of var)))))


(defun unbind-$ (var)
    "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
  (declare (type tvar var))

  (if (recording?)
      (tx-write-of var +unbound-tvar+)
      (setf (raw-value-of var) +unbound-tvar+))
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
    (if (eq value +unbound-tvar+)
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
        (if (eq value +unbound-tvar+)
            (values default nil)
            (progn
              (tx-write-of var +unbound-tvar+)
              (values value t))))
      (let1 value (raw-value-of var)
        (if (eq value +unbound+)
            (values default nil)
            (progn
              (setf (raw-value-of var) +unbound-tvar+)
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
        (if (eq old-value +unbound-tvar+)
            (values (tx-write-of var value) t)
            (values default nil)))
      (let1 old-value (raw-value-of var)
        (if (eq old-value +unbound-tvar+)
            (values (setf (raw-value-of var) value) t)
            (values default nil)))))

;;;; ** Accessors


(defmethod value-of ((var tvar))
  "Return the value inside a TVAR. Works both outside and inside transactions."
  ($ var))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a TVAR. Works both outside and inside transactions."
  (setf ($ var) value))




;;;; ** Listening and notifying

(defun listen-tvar (var log)
  "Add LOG to VAR's waiting list, so that LOG will be notified if VAR changes."

  (declare (type tvar var)
           (type tlog log))
  (with-lock-held ((tvar-waiting-lock var))
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
  (with-lock-held ((tvar-waiting-lock var))
    (awhen (tvar-waiting-for var)
      (remhash log it))))


(defun notify-tvar (var)
  "Wake up all threads waiting for VAR to change."

  (declare (type tvar var))
  (with-lock-held ((tvar-waiting-lock var))
    (awhen (tvar-waiting-for var)
      (do-hash (log) it
        (notify-tlog log var)))))


;;;; ** Printing

(defprint-object (obj tvar)
  (multiple-value-bind (value present?) (peek-$ obj)
    (if present?
        (format t "~A [~A]" (~ obj) value)
        (format t "~A unbound" (~ obj)))))
