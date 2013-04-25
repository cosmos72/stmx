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
        #:bordeaux-threads
        #:fiveam
        #:stmx.lang
        #:stmx
        #:stmx.util)

  (:import-from #:stmx
                #:raw-value-of #:tvar-versioned-value #:+invalid-counter+
                #:tx-read-of #:tx-write-of
                #:tlog  #:make-tlog
                #:rerun-error #:rerun
                #:retry-error #:retry
                #:commit
                #:valid?
                #:current-tlog
                #:with-recording-to-tlog)

  (:import-from #:stmx.util
                #:print-object-contents
                #:print-bmap
                #:bnode #:rbnode #:tnode #:color-of
                #:bmap/new-node
                #:+red+ #:+black+
                #:red? #:black?)

  (:export #:suite))


(in-package :stmx.test)

(fiveam:def-suite suite)

(defun configure-log4cl ()
  (log:config :clear :sane :info :this-console :pattern "[%D{%H:%M:%S}] [%-5P] {%t} <%c{}{}{:downcase}> %m%n"))
