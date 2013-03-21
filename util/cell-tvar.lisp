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


(in-package :stmx.util)

;;;; ** Concurrent cell implemented with a TVAR

;;; Max: here we could use the same trick as cell-tobj.lisp:
;;; a special *empty-tvar* value to mean "cell is empty".
;;; Anyway, using tvar functions bound-$? and unbind-$ is less verbose
;;;
;;; Max: for illustration purposes, here we use (defmethod ... (atomic ... ))
;;; instead of (transaction (defmethod ...)) - they are equivalent.

(defmethod empty? ((var tvar))
  (atomic
    (bound-$? var)))

(defmethod empty! ((var tvar))
  "Remove value from tvar."
  (atomic
    (unbind-$ var)))

;; no need to specialize (full?) on TVARs: the method in cell.lisp is enough
#|
(defmethod full? ((var tvar))
  (not (empty? var)))
|#

(defmethod take ((var tvar))
  (atomic
    (if (empty? var)
        (retry)
        (prog1 ($ var)
          (empty! var)))))

(defmethod put ((var tvar) value)
  (atomic
    (if (empty? var)
        (setf ($ var) value)
        (retry))))

(defmethod try-take ((var tvar))
   "hand-made, nonblocking version of (take place) for TVARs.
less general but probably faster than the unspecialized (try-take place)
which calls (atomic (nonblocking (take place)))"
   (atomic
    (if (empty? var)
        nil
        (let1 value ($ var)
          (empty! var)
          (values t value)))))

(defmethod try-put ((var tvar) value)
   "hand-made, nonblocking version of (put place) for TVARs.
less general but probably faster than the unspecialized (try-put place)
which calls (atomic (nonblocking (put place value)))"
   (atomic
    (if (empty? var)
        (progn
          (setf ($ var) value)
          (values t value))
        nil)))
