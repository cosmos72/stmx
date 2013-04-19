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


(in-package :stmx.test)


(eval-always
  (let ((pkg (find-package (symbol-name 'stmx.util)))
        (of (symbol-name '-of)))

    (defmacro _ (obj slot-name)
      "Use slot names defined in package STMX.UTIL"
      `(slot-value ,obj ',(intern (symbol-name slot-name) pkg)))

    #+never
    (defmacro _ (obj slot-name)
      "Use slot accessors defined in package STMX.UTIL"
      (let1 accessor (intern (concatenate 'string (symbol-name slot-name) of) pkg)
        `(,accessor ,obj)))))
