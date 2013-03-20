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

;;;; ** Concurrent cell implemented with a raw TVAR

;;; Max: here we could use the same trick as cell-tobj.lisp:
;;; a special *empty-tvar* value to mean "cell is empty".
;;; Anyway, using tvar functions bound-$? and unbind-$ is less verbose

(defmethod empty? ((var tvar))
  (atomic
    (bound-$? var)))

(defmethod empty! ((var tvar))
  "Remove value from tvar."
  (atomic
    (unbind-$ var)))

(defmethod full? ((var tvar))
  (not (empty? var)))

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

(defmethod try-take ((cell cell))
  (atomic
    (nonblocking
     (take cell))))

(defmethod try-put ((cell cell) val)
  (atomic
    (nonblocking
     (put cell val))))
