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


(in-package :cl-user)

(defpackage #:stmx.example.bank-account.lock
  (:use #:cl)

  (:import-from #:stmx.lang
                #:new #:defprint-object))

(in-package :stmx.example.bank-account.lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype lock () 't)

(defun make-lock (&optional name)
  (bt:make-lock name))

(defmacro with-lock ((lock) &body body)
  `(bt:with-lock-held (,lock)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype unsigned-fixnum () '(integer 0 #.most-positive-fixnum))

(defclass account ()
  ((balance :initform 0  :initarg :balance  :type unsigned-fixnum :accessor account-balance)
   (lock    :initform (make-lock "ACCOUNT") :type lock            :reader   account-lock)
   (name    :initform "" :initarg :name     :type string          :reader   account-name)))


(defprint-object (obj account :identity nil)
    (format t "~S ~S" (account-name obj) (account-balance obj)))


(defmacro with-account-lock ((account) &body body)
  `(with-lock ((account-lock ,account))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun withdraw (delta account)
  "decrease ACCOUNT balance by DELTA. return T if successful"
  (declare (type unsigned-fixnum delta)
           (type account account))

  (with-account-lock (account)
    (when (>= (account-balance account) delta)
      (decf (account-balance account) delta)
      t)))


(defun deposit (delta account)
  "increase ACCOUNT balance by DELTA. return T if successful"
  (declare (type unsigned-fixnum delta)
           (type account account))

  (with-account-lock (account)
    (when (<= (account-balance account) (- most-positive-fixnum delta))
      (incf (account-balance account) delta)
      t)))


(defun transfer-broken (delta account1 account2)
  "transfer DELTA from ACCOUNT1 to ACCOUNT2. return T if successful."
  (declare (type unsigned-fixnum delta)
           (type account account1 account2))

  (when (withdraw delta account1)
    (if (deposit delta account2)
        t
        (if (deposit delta account1)
            t
            (error "cannot deposit ~S back into ~S or in ~S!" delta account1 account2)))))


(defun transfer-deadlock-ugly (delta account1 account2)
  "transfer DELTA from ACCOUNT1 to ACCOUNT2. return t if successful."
  (declare (type unsigned-fixnum delta)
           (type account account1 account2))

  (with-account-lock (account1)
    (with-account-lock (account2)
      (when (withdraw delta account1)
        (if (deposit delta account2)
            t
            (if (deposit delta account1)
                t
                (error "cannot happen! cannot deposit ~S back into ~S!" delta account1)))))))


(defun transfer-deadlock-not-oo (delta account1 account2)
  "transfer delta from account1 to account2. return t if successful."
  (declare (type unsigned-fixnum delta)
           (type account account1 account2))

  (with-account-lock (account1)
    (with-account-lock (account2)
      (when (>= (account-balance account1) delta)
        (when (<= (account-balance account2) (- most-positive-fixnum delta))
          (decf (account-balance account1) delta)
          (incf (account-balance account2) delta)
          t)))))



(defun transfer (delta account1 account2)
  (declare (type unsigned-fixnum delta)
           (type account account1 account2))

  (transfer-deadlock-not-oo delta account1 account2))


(defparameter *account1* (new 'account :name "Mario rossi" :balance 1000))
(defparameter *account2* (new 'account :name "Giuseppe Verdi"))

(defun test-bank-accounts (&optional (account1 *account1*) (account2 *account2*))
  (log:info (transfer 700 account1 account2))
  (log:info (transfer 500 account2 account1)))

