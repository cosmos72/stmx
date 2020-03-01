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

(defpackage #:stmx.lang
  (:use #:cl
        #:bordeaux-threads)

  (:export #:define-global    #:define-constant-once

           #:do-tree
           #:stringify    #:concat-symbols
           #:with-gensym  #:with-gensyms
           #:eval-always  #:new          #:let1
           #:when-bind    #:awhen
           #:if-bind      #:aif

           #:enable-#?-syntax
           #:set-feature  #:set-features #:default-feature #:default-features
           #:get-feature  #:all-features #:assoc-feature

           #:log.trace    #:log.debug    #:log.make-logger

           ;; bordeaux-threads helpers
           #:start-multithreading
           #:start-thread #:wait4-thread
           #:*current-thread*
           #:with-lock
           #:ensure-thread-initial-binding
           #:ensure-thread-initial-bindings
           #:save-thread-initial-bindings

           ;; cons pool
           #:cons^ #:free-cons^ #:free-list^
           #:push^ #:pop-free-cons^

           ;; hardware memory transactions. see also feature #?+hw-transactions
           #:hw-transaction-supported?
           #:hw-transaction-begin
           #:hw-transaction-running?
           #:hw-transaction-abort
           #:hw-transaction-end
           #:hw-transaction-rerun-may-succeed?
           ;; returned by (hw-transaction-begin) if successful
           #:+hw-transaction-started+
           ;; cached result of (hw-transaction-supported?)
           #:+hw-transaction-supported+
           ;; equivalent to (and +hw-transaction-supported+ (hw-transaction-running?))
           #:hw-transaction-supported-and-running?

           ;; atomic operations
           #:atomic-num
           #:atomic-incf #:atomic-decf
           #:atomic-compare-and-swap  #:atomic-pop
           #:mem-read-barrier #:mem-write-barrier

           #:atomic-counter-slot-type #:atomic-counter-num
           #:atomic-counter #:make-atomic-counter
           #:atomic-counter-mutex ;; exists only if feature #?+fast-atomic-counter is not set

           #:incf-atomic-counter #:incf-atomic-place
           #:set-atomic-counter  #:set-atomic-place
           #:get-atomic-counter  #:get-atomic-place
           #:get-atomic-counter-plus-delta #:get-atomic-place-plus-delta


           #:mutex             #:make-mutex
           #:mutex-owner       #:mutex-lock
           #:try-acquire-mutex #:try-acquire-mutex/catch-recursion
           #:release-mutex
           #:mutex-is-free?    #:mutex-is-own?
           #:mutex-is-own-or-free?

           #:fast-vector        #:make-fast-vector
           #:fast-vector-length #:fast-vector-capacity
           #:fast-vector-pop    #:fast-vector-pop-macro
           #:fast-vector-push   #:fast-vector-push-extend
           #:fast-vector-clear  #:do-fast-vector

           #:get-hash #:set-hash ;; also (setf get-hash)
           #:rem-hash #:clear-hash #:do-hash

           #:hash-table-keys  #:hash-table-values  #:hash-table-pairs
           #:copy-hash-table  #:merge-hash-tables

           #:id-of ;; also (setf id-of)
           #:~     ;; also (setf ~)

           #:defprint-object

           #:clos-compute-class-precedence-list))
