;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :cl-user)

(defpackage #:stmx.example.bank-account.stmx
  (:use #:cl #:stmx)

  (:import-from #:stmx.lang
                #:new #:defprint-object))

(in-package :stmx.example.bank-account.stmx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype unsigned-fixnum () '(integer 0 #.most-positive-fixnum))

(transactional
 (defclass account ()
   ((balance :initform 0  :initarg :balance  :type unsigned-fixnum :accessor account-balance)
    (name    :initform "" :initarg :name     :type string          :reader   account-name
             :transactional nil))))


(defprint-object (obj account :identity nil)
    (format t "~S ~S" (account-name obj) (account-balance obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun withdraw (delta account)
  "decrease ACCOUNT balance by DELTA. return T if successful"
  (declare (type unsigned-fixnum delta)
           (type account account))

  (atomic
   (when (>= (account-balance account) delta)
     (decf (account-balance account) delta)
     t)))


(defun deposit (delta account)
  "increase ACCOUNT balance by DELTA. return T if successful"
  (declare (type unsigned-fixnum delta)
           (type account account))

  (atomic
   (when (<= (account-balance account) (- most-positive-fixnum delta))
     (incf (account-balance account) delta)
     t)))


(defun transfer (delta account1 account2)
  "transfer DELTA from ACCOUNT1 to ACCOUNT2. return t if successful."
  (declare (type unsigned-fixnum delta)
           (type account account1 account2))

  (atomic
   (when (withdraw delta account1)
     (if (deposit delta account2)
         t
         (if (deposit delta account1)
            t
            (error "cannot happen! cannot deposit ~S back into ~S!" delta account1))))))

  
(defparameter *account1* (new 'account :name "Mario rossi" :balance 1000))
(defparameter *account2* (new 'account :name "Giuseppe Verdi"))

(defun test-bank-accounts (&optional (account1 *account1*) (account2 *account2*))
  (log:info (transfer 700 account1 account2))
  (log:info (transfer 500 account2 account1)))

