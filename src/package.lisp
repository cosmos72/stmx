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


;;;; * STMX

(in-package :cl-user)

(defpackage :stmx
  (:use :cl
        :arnesi
        :bordeaux-threads
        :closer-mop)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :standard-class
                          :standard-generic-function
                          :defmethod
                          :defgeneric)
  (:export :atomic
           :retry
           :orelse

           :transactional
           :transaction

           ;; utilities
           :nonblocking
           :before-commit  :call-before-commit
           :after-commit   :call-after-commit

           ;; metaclasses
           :transactional-object
           :transactional-class

           ;; low-level API to use TVARs directly
           :tvar
           :$
           :bound-$?
           :unbind-$))
