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

  (:export #:atomic    #:run-atomic
           #:retry
           #:orelse    #:run-orelse

           ;; hardware transactions. still experimental
           #:hw-atomic

           #:transactional
           #:transaction
           #:transaction?

           ;; utilities
           #:nonblocking
           #:before-commit  #:call-before-commit
           #:after-commit   #:call-after-commit

           ;; metaclasses
           #:transactional-object
           #:transactional-class

           ;; low-level API to use TVARs directly
           #:tvar #:+unbound-tvar+
           #:$ #:$-tx #:$-hwtx #:$-notx ;; also (setf $) (setf $-tx) (setf $-hwtx) and (setf $-notx)

           #:$-noerror ;; same as ($ VAR) but returns +unbound-tvar+ if VAR is unbound
           #:bound-$? #:unbind-$

           #:fast-atomic))

