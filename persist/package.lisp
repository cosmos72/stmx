;; -*- lisp -*-

;; This file is part of STMX-PERSIST.
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


;;;; * STMX-PERSIST

(in-package :cl-user)

(defpackage #:stmx-persist

  (:nicknames #:sp)

  (:use #:cl)

  (:export #:+null-pointer+ #:+bad-fd+
           #:open-fd #:close-fd #:mmap #:munmap
           #:mget-primitive #:mset-primitive
           #:mwrite-n-chars #:mwrite-n-byte-chars))


