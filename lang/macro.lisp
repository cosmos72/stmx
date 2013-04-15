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


(in-package :stmx.lang)


;;;; * Miscellaneous macros

(defmacro with-gensym (name &body body)
  `(let ((,name (gensym)))
     ,@body))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))


;;;; ** A minimal clean-room reimplementation of some macros found in ARNESI


(defmacro new (class &rest initargs &key &allow-other-keys)
  `(make-instance ,class ,@initargs))


(defmacro eval-always (&rest body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


(defmacro let1 (var value &rest body)
  `(let ((,var ,value))
     ,@body))


(defmacro when-bind (var test &rest body)
  `(let1 ,var ,test
     (when ,var
       ,@body)))


(defvar +it+ (symbol-name 'it))

(defmacro awhen (test &rest body)
  (let1 it (intern +it+ *package*)
    `(when-bind ,it ,test
       ,@body)))


(defmacro if-bind (var test then &optional else)
  `(let1 ,var ,test
     (if ,var
         ,then
         ,else)))


(defmacro aif (test then &optional else)
  (let1 it (intern +it+ *package*)
    `(if-bind ,it ,test
       ,then
       ,else)))

