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

(eval-when (:compile-toplevel :load-toplevel)

  (defun compile-if-error (ch args)
    (error "#?~a must be followed by one of: symbol, (or ...), (and ...), (not ...),
  (eql ...), (symbol ...). found instead ~S"
           ch args))

  (defun compile-ensure-1-arg (ch args)
    (when (or (null (cdr args)) (cddr args))
      (error "#?~a(~a ...) must must have exactly one argument. found instead ~S"
             ch (first args) args)))

  (defun compile-ensure-2-args (ch args)
    (when (or (null (cddr args)) (cdddr args))
      (error "#?~a(~a ...) must must have exactly two arguments. found instead ~S"
             ch (first args) args)))


  (declaim (ftype (function (t t) boolean) compile-if-eval))

  (defun compile-if-and (ch args)
    "Return T if all the features in ARGS are present in *FEATURE-LIST*, otherwise return NIL."
    (declare (type list args))
    (loop for arg in (rest args) ;; skip 'and
       always (compile-if-eval ch arg)))


  (defun compile-if-or (ch args)
    "Return T if at least one feature in ARGS is present in *FEATURE-LIST*, otherwise return NIL."
    (declare (type list args))
    (loop for arg in (rest args) ;; skip 'or
       thereis (compile-if-eval ch arg)))


  (defun compile-if-not (ch args)
    "Return NIL if the feature in ARGS is present in *FEATURE-LIST*, otherwise return T."
    (declare (type list args))

    (compile-ensure-1-arg ch args)
    (pop args) ;; skip 'not

    (not (compile-if-eval ch (first args))))


  (defun compile-if-eql (ch args)
    "Return T if feature has the specified value, otherwise return NIL."
    (declare (type list args))

    (compile-ensure-2-args ch args)
    (pop args) ;; skip 'eql

    (let* ((feature (pop args))
           (value   (pop args)))
      (eql value (get-feature feature))))


  (defun compile-if-symbol (ch args)
    "Return T if symbol exists in specified package, otherwise return NIL.
Arguments are: package-name symbol-name."
    (declare (type list args))

    (compile-ensure-2-args ch args)
    (pop args) ;; skip 'symbol

    (let* ((pkg-name    (pop args))
           (symbol-name (pop args)))
      (declare (type symbol pkg-name symbol-name))
      (when-bind pkg (find-package pkg-name)
                 (if (nth-value 1 (find-symbol (symbol-name symbol-name) pkg))
                     t
                     nil))))


  (defun compile-if-eval (ch args)
    (the (values boolean &optional)
      (cond
        ((symbolp args)            (feature? args))
        ((not (listp args))        (compile-if-error  ch args))
        ((eq 'and    (first args)) (compile-if-and    ch args))
        ((eq 'or     (first args)) (compile-if-or     ch args))
        ((eq 'not    (first args)) (compile-if-not    ch args))
        ((eq 'eql    (first args)) (compile-if-eql    ch args))
        ((eq 'symbol (first args)) (compile-if-symbol ch args))
        (t                         (compile-if-error  ch args)))))


  (defun compile-if-transformer (stream subchar arg)
    (declare (ignore subchar arg))
    (let* ((ch   (read-char stream t))
           (ch-flag (ecase ch
                      (#\+ t)
                      (#\-  nil)))
           (args (read stream t)))
      
      (if (or *read-suppress* (eq ch-flag (compile-if-eval ch args)))
          (read stream t)
          (let ((*read-suppress* t))
            (read stream t)
            (values)))))


  (defun %enable-#?-syntax ()
    (setf *readtable* (copy-readtable))
    (set-dispatch-macro-character #\# #\? #'compile-if-transformer))

  (defmacro enable-#?-syntax ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%enable-#?-syntax))))
