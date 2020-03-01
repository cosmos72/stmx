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


;;;; * STMX

(in-package :cl-user)

(defpackage #:stmx

  (:use #:cl
        #:bordeaux-threads
        #:closer-mop
        #:stmx.lang)

  (:shadowing-import-from #:closer-mop
                          #:defclass
                          #:standard-class
                          #:standard-generic-function
                          #:standard-method
                          #:defmethod
                          #:defgeneric)

  ;; no need for closer-mop version of typep and subtypep;
  ;; they even cause some tests to fail
  #+cmucl
  (:shadowing-import-from #:cl
                          #:typep
                          #:subtypep)

  (:export #:*stmx-version*

           #:atomic    #:run-atomic
           #:retry
           #:orelse    #:run-orelse

           ;; hardware transactions.
           #:hw-atomic2
	   #:hw-transaction-supported?
	   #:hw-transaction-supported-and-running?

           ;; defining classes, structs, functions and methods
           #:transactional      #:transaction          #:transaction?
           #:transactional-class

           #:transactional-struct #:non-transactional-struct #:analyze-struct

           ;; utilities
           #:nonblocking
           #:before-commit  #:call-before-commit
           #:after-commit   #:call-after-commit

           ;; metaclasses
           #:transactional-object
           #:transactional-class

           ;; low-level API to use TVARs directly
           #:tvar #:+unbound-tvar+ #:$-slot #:$
	   ;; also (setf $-slot) (setf $)

           #:bound-$? #:unbind-$

           ;; helpers to optimize code
           #:fast-atomic #:optimize-for-transaction #:optimize-for-transaction*))

