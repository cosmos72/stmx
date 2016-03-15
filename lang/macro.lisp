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


(in-package :stmx.lang)


;;;; * Miscellaneous macros

(defun visit-tree (function tree &optional result)
  (let ((function (coerce function 'function)))
    (labels ((%visit-tree (tree)
               (dolist (e tree)
                 (if (consp e)
                     (%visit-tree e)
                     (funcall function e)))))
      (%visit-tree tree)
      result)))
  

(defmacro do-tree ((atom tree &optional result) &body body)
  "Execute BODY for each atom inside TREE"
  `(visit-tree (lambda (,atom) ,@body) ,tree ,result))


(defun stringify (&rest things)
  "Print the things to a string and return it"
  (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (*print-array*    t)
        (*print-base*     10)
        (*print-escape*   nil)
        (*print-gensym*   nil)
        (*print-pretty*   nil)
        (*print-radix*    nil)
        (*print-readably* nil))
    (do-tree (thing things s)
      (format s "~A" thing))))

(defun concat-symbols (&rest things)
  "Print the things to a string, the convert the string into a symbol interned in current package.
Return the symbol"
  (values (intern (apply #'stringify things) *package*)))



(defmacro with-gensym (name &body body)
  (let ((sym (if (consp name) (first name) name))
        (str (if (consp name) `(stringify ,@(rest name) '-) (stringify name '-))))
    `(let ((,sym (gensym ,str)))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names
            for sym = (if (consp name) (first name) name)
            for str = (if (consp name) `(stringify ,@(rest name) '-) (stringify name '-))
            collect `(,sym (gensym ,str)))
     ,@body))

;;;; ** A minimal clean-room reimplementation of some macros found in ARNESI


(defmacro new (class &rest initargs &key &allow-other-keys)
  `(make-instance ,class ,@initargs))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


(defmacro let1 (var value &body body)
  `(let ((,var ,value))
     ,@body))


(defmacro when-bind (var test &body body)
  `(let1 ,var ,test
     (when ,var
       ,@body)))


(defvar +it+ (symbol-name 'it))

(defmacro awhen (test &body body)
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

;;;; ** Logging macros.
;;
;; Yes, I am so concerned with speed that even wasting some nanoseconds
;; in a disabled (log:trace) is annoying


#|
(defmacro log.debug (&rest args)
  `(log:debug ,@args))

(defmacro log.trace (&rest args)
  `(log:trace ,@args))

(defmacro log.make-logger (&rest args)
  `(log:make ,@args))
|#


(defmacro log.debug (&rest args)
  (declare (ignore args))
  nil)

(defmacro log.trace (&rest args)
  (declare (ignore args))
  nil)

(defmacro log.make-logger (&rest args)
  (declare (ignore args))
  nil)
