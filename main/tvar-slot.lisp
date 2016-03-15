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

(optimize-for-transaction
  (defun $-slot (var)
   "Get the value from the transactional variable VAR and return it.
Signal an error if VAR is not bound to a value.

Works both inside and outside transactions.
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value ($ var)
    (unless (eq value +unbound-tvar+)
      (return-from $-slot value))
    (unbound-tvar-error var))))


(optimize-for-transaction*
 (:inline t)
 (defun (setf $-slot) (value var)
   (declare (type tvar var))
   (setf ($ var) value)))


;;;; ** Accessors


(defgeneric value-of (place))
(defgeneric (setf value-of) (value place))

(defmethod value-of ((var tvar))
  "Return the value inside a TVAR. Works both outside and inside transactions.
Equivalent to ($-slot var)"
  ($-slot var))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a TVAR. Works both outside and inside transactions.
Equivalent to (setf ($-slot var) value)"
  (setf ($-slot var) value))



(optimize-for-transaction
 (defun bound-$? (var)
   "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
   (declare (type tvar var))

   (not (eq +unbound-tvar+ ($ var)))))


(optimize-for-transaction
 (defun unbind-$ (var)
   "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
   (declare (type tvar var))

   (setf ($ var) +unbound-tvar+)
   var))


(optimize-for-transaction
 (defun peek-$ (var &optional default)
   "Get the value from the transactional variable VAR
and return it and t as multiple values.
If VAR is not bound to a value, return (values DEFAULT nil).

Works both inside and outside transactions."
   (declare (type tvar var))

   (let1 value ($ var)
     (if (eq value +unbound-tvar+)
         (values default nil)
         (values value   t)))))


(optimize-for-transaction
 (defun try-take-$ (var &optional default)
   "Get the value from the transactional variable VAR,
unbind it and and return t and the original value as multiple values.
If VAR is not bound to a value, return (values nil DEFAULT).

Works both inside and outside transactions."
   (declare (type tvar var))

   (let1 value ($ var)
     (if (eq value +unbound-tvar+)
         (values nil default)
         (progn
           (setf ($ var) +unbound-tvar+)
           (values t value))))))


(optimize-for-transaction
 (defun try-put-$ (var value &optional default)
   "If VAR is not bound, bind it to VALUE and return (values VALUE t)
If VAR is already bound to a value, return (values DEFAULT nil).

Works only inside software transactions."
   (declare (type tvar var))

   (let1 old-value ($ var)
     (if (eq old-value +unbound-tvar+)
         (values t (setf ($ var) value))
         (values nil default)))))
