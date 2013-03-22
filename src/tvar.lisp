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

(defun read-tvar (var &optional (log (current-tlog)))
  "Record the reading of VAR to LOG and return VAR value.
READ-TVAR is an internal function called by ($ VAR) and by reading TOBJs slots."
  (declare (type tvar var)
           (type tlog log))
  (multiple-value-bind (value value?)
      (gethash var (writes-of log))
    (when value?
      (return-from read-tvar value)))

  (multiple-value-bind (value value?)
      (gethash var (reads-of log))
    (when value?
      (return-from read-tvar value)))

  (setf (gethash var (reads-of log)) (raw-value-of var)))


(defun write-tvar (var value &optional (log (current-tlog)))
  "Record in LOG the writing of VALUE into VAR; return VALUE.
WRITE-TVAR is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))
  (setf (gethash var (writes-of log)) value))



(defun $ (var)
    "Get the value from the transactional variable VAR.
Works both outside and inside transactions.

During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value (if (recording?)
                  (read-tvar var)
                  (raw-value-of var))
    (if (eq value +unbound+)
        (unbound-tvar-error var)
        value)))


(defun (setf $) (value var)
    "Store VALUE inside transactional variable VAR.
Works both outside and inside transactions.

During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  (if (recording?)
      (write-tvar var value)
      (setf (raw-value-of var) value)))
               
            
  
(defun bound-$? (var)
    "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (not (eq +unbound+
           (if (recording?)
               (read-tvar var)
               (raw-value-of var)))))


(defun unbind-$ (var)
    "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
  (declare (type tvar var))

  (if (recording?)
      (write-tvar var +unbound+)
      (setf (raw-value-of var) +unbound+))
  var)




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
  (with-lock-held ((waiting-lock-of var))
    (setf (gethash log (waiting-for var)) t)))


(defun unlisten-tvar (var log)
  "Remove LOG from VAR's waiting list, so that LOG will *not* be notified anymore
if VAR changes."

  (declare (type tvar var)
	   (type tlog log))
  (with-lock-held ((waiting-lock-of var))
    (remhash log (waiting-for var))))


(defun notify-tvar (var)
  "Wake up all threads waiting for VAR to change."

  (declare (type tvar var))
  (with-lock-held ((waiting-lock-of var))
    (dohash (waiting-for var) log dummy
      (notify-tlog log var))))


;;;; ** Printing

(defprint-object (obj tvar)
  (if (bound-$? obj)
      (format t "[~A]" ($ obj))
      (format t "<unbound>")))
