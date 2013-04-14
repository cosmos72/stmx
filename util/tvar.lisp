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

;;;; ** Transactional cell implemented with a TVAR

;;; Max: we could use the same trick as in tcell.lisp:
;;; a special *empty-tvar* value to mean "cell is empty".
;;; Anyway, using tvar functions bound-$? and unbind-$ is less verbose
;;; and feels more "natural".


;; no need to wrap empty? in a transaction:
;; bound-$? is atomic, transaction aware, and performs a single read
(defmethod empty? ((var tvar))
  (not (bound-$? var)))

(transaction
 (defmethod empty! ((var tvar))
   "Remove value from tvar."
   (unbind-$ var)))

;; no need to specialize (full?) on TVARs: the method in cell.lisp is enough
;;
;; (defmethod full? ((var tvar))
;;   (not (empty? var)))


(transaction
 (defmethod peek ((var tvar) &optional default)
   (peek-$ var default)))

(transaction
 (defmethod take ((var tvar))
   (multiple-value-bind (value took?) (try-take-$ var)
     (unless took?
       (retry))
     value)))


(transaction
 (defmethod put ((var tvar) value)
   (unless (nth-value 1 (try-put-$ var value))
     (retry))
   value))

(transaction
 (defmethod try-take ((var tvar))
   "hand-made, nonblocking version of (take place) for TVARs.
Less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-take place) which calls
\(atomic (nonblocking (take place)))"
   (multiple-value-bind (value took?) (try-take-$ var)
     (values took? value))))

(transaction
 (defmethod try-put ((var tvar) value)
   "hand-made, nonblocking version of (put place) for TVARs.
Less general but approx. 3 times faster (on SBCL 1.0.57.0.debian,
Linux amd64) than the unspecialized (try-put place) which calls
\(atomic (nonblocking (put place value)))"
   (multiple-value-bind (value-or-nil put?) (try-put-$ var value)
     (values put? value-or-nil))))
