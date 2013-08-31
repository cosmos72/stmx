;; -*- lisp -*-

;; This file is part of SB-TRANSACTION.
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


;; this code is VERY non-portable:
;;
;; it only builds with SBCL running on a x86-64 CPU,
;; and (except for CPUID and TRANSACTION-SUPPORTED-P) defines CPU instructions
;; that require RTM support, i.e. hardware memory transactions.
;;
;; As of June 2013, the only x86-64 CPUs supporting RTM are:
;; * Intel Core i5 4570
;; * Intel Core i5 4670
;; * Intel Core i7 4770


(in-package :cl-user)

(defpackage #:sb-transaction
  (:use #:cl)

  (:export #:cpuid  #:lock-elision-supported-p  #:transaction-supported-p

           #:transaction-begin #:transaction-end
           #:transaction-abort #:transaction-running-p
           #:transaction-rerun-may-succeed-p

           #:+transaction-started+ #:+transaction-user-abort+))

