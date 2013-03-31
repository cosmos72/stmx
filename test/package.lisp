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

(defpackage #:stmx.test
  (:use #:cl
        #:arnesi
        #:bordeaux-threads
        #:fiveam
        #:stmx
        #:stmx.util)

  (:import-from #:stmx
                #:new
                #:lock-of
                #:raw-value-of
                #:tx-read-of
                #:tx-write-of
                #:tlog
                #:commit
                #:valid?
                #:current-tlog
                #:with-recording-to-tlog

                #:with-ro-slots

                #:get-hash #:set-hash #:rem-hash)

  (:shadowing-import-from #:stmx
                          #:hash-table-keys #:hash-table-values #:hash-table-pairs)

  (:import-from #:stmx.util
                #:bnode
                #:red? #:black?)

  (:export #:suite))


(in-package :stmx.test)

(fiveam:def-suite suite)

(defun configure-log4cl ()
  (log:config :info :sane :this-console :pattern "[%D{%H:%M:%S}] [%-5P] {%t} <%c{}{}{:downcase}> %m%n"))
