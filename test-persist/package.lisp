;; -*- lisp -*-

;; This file is part of STMX-PERSIST.TEST.
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


;;;; * STMX-PERSIST.TEST

(in-package :cl-user)

(defpackage #:stmx-persist.test

  (:nicknames #:sp.test)

  (:use #:cl
        #:stmx-persist)

  (:export #:suite))




(in-package :stmx-persist.test)

(fiveam:def-suite suite)
