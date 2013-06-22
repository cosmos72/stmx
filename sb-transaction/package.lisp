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


;;;; * SBCL-TRANSACTION

(in-package :cl-user)

(defpackage #:sb-transaction
  (:use #:cl)

  #+x86-64
  (:export #:cpuid  #:cpuid-transaction-supported-p

           #:transaction-begin #:transaction-end
           #:transaction-abort #:transaction-running-p))
