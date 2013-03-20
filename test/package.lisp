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


(in-package :cl-user)

(defpackage :stmx.test
  (:use :cl
        :arnesi
        :fiveam
        :stmx
        :stmx.util)

  (:import-from :bordeaux-threads
                :make-thread
                :join-thread)

  (:import-from :stmx
                :lock-of
                :raw-value-of
                :read-tvar
                :write-tvar
                :tlog
                :commit
                :valid?
                :current-tlog
                :with-recording-to-tlog)

  (:export :suite))


(in-package :stmx.test)

(fiveam:def-suite suite)
