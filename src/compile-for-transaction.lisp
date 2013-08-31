;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013 massimiliano ghilardi
;;
;; this library is free software: you can redistribute it and/or
;; modify it under the terms of the lisp lesser general public license
;; (http://opensource.franz.com/preamble.html), known as the llgpl.
;;
;; this library is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.
;; see the lisp lesser general public license for more details.


(in-package :stmx)


(declaim (type (member nil :hwtx :swtx :notx) *compile-for*))
(defvar *compile-for* nil)

(defmacro $ (var &optional version)
  (case *compile-for*
    (:hwtx `(log:info "$/hwtx ~S ~S" ,var ,version)) ;;,@(when version `(,version))))
    (:swtx `(log:info "$/swtx ~S" ,var))
    (:notx `(log:info "$/notx ~S" ,var))
    (t     `(log:info "$!     ~S" ,var))))


(defmacro compile-for (which form)
  (setf *compile-for* which)
  form)

(defmacro compile-for-transaction (form)
  `(eval-always
     (compile-for :hwtx ,form)
     (compile-for :swtx ,form)
     (compile-for :notx ,form)
     (compile-for nil   ,form)))

(defvar *v* (tvar 0))
(compile-for-transaction ($ *v* 7))