;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013-2014 Massimiliano Ghilardi
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

;;;; ** constants

(declaim (type cons *stmx-version*))
(defparameter *stmx-version* '(1 9 1))


(defun stmx-internal-error (datum &rest arguments)
  (let ((datum
         (concatenate 'string "STMX internal error!~%" datum
                      "you may have discovered a bug in STMX, or in one of its dependencies,
     or in the Lisp compiler (unlikely, but possible).
     Please check if it's a known bug at https://github.com/cosmos72/stmx/issues
     If not already present, you can report it. Include at least the following:
     * CPU model
     * Operating system, including version
     * Lisp compiler, including version
     * STMX version: report both stmx:*stmx-version* and (ql:system-apropos \"stmx\") 
     * a short, self-contained example that causes the bug.")))
    (apply #'error datum arguments)))
    


