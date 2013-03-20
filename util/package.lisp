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


;;;; * STMX.UTIL

(in-package :cl-user)

(defpackage :stmx.util
  
  (:use :cl
        :arnesi
        :stmx)

  (:export :cell
           :empty?
           :empty!
           :full?
           :take
           :put
           :try-put
           :try-take

           :counter
           :count-of
           :increment
           :decrement
           :reset
           :swap

           :tqueue
           :deq
           :enq
           :empty-queue?
           :full-queue?
           :qlength

           :chan
           :port
           :read-port
           :write-chan))
