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

(defpackage #:stmx.lang
  (:use #:cl
        #:bordeaux-threads)

  (:export #:with-gensym  #:with-gensyms
           #:eval-always  #:new     #:let1
           #:when-bind    #:awhen
           #:if-bind      #:aif

           #:log.trace    #:log.debug

           #:atomic-counter #:make-atomic-counter
           #:get-atomic-counter #:incf-atomic-counter

           #:lock-rw             #:make-lock-rw        
           #:try-acquire-lock-rw #:release-lock-rw
           #:try-acquire-lock-ro #:release-lock-ro

           #:fast-vector        #:make-fast-vector
           #:fast-vector-length #:fast-vector-max-length
           #:fast-vector-push   #:fast-vector-pop #:fast-vector-pop-macro
           
           #:get-hash #:set-hash ;; also (setf get-hash)
           #:rem-hash #:clear-hash #:do-hash

           #:hash-table-keys  #:hash-table-values  #:hash-table-pairs
           #:copy-hash-table  #:merge-hash-tables

           ;; bordeaux-threads wrappers
           #:start-thread #:wait4-thread

           #:id-of ;; also (setf id-of)
           #:~     ;; also (setf ~)

           #:defprint-object

           #:clos-compute-class-precedence-list))
