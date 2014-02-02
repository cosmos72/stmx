;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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

;;;; ** Transactional stack (first-in-last-out) buffer

(transactional
 (defclass tstack ()
   ((top  :type list :initform nil :accessor top-of))))

(declaim (ftype (function () (values tstack &optional)) tstack))

(defun tstack ()
  "Create and return a new TSTACK."
  (new 'tstack))


(defmethod empty? ((s tstack))
  (null (_ s top)))

(transaction
 (defmethod empty! ((s tstack))
   (setf (_ s top) nil)
   s))

(defmethod full? ((s tstack))
  "A tstack is never full, so this method always returns nil."
  nil)
                  

(transaction
 (defmethod peek ((s tstack) &optional default)
   "Return the first value in tstack S without removing it, and t as multiple values.
Return (values DEFAULT nil) if S contains no values."
   (with-ro-slots (top) s
     (if (null top)
         (values default nil)
         (values (first top) t)))))


(transaction
 (defmethod take ((s tstack))
   "Wait until tstack S contains at least one value, then remove
and return the first value."
   (with-rw-slots (top) s
     (if (null top)
         (retry)
         (pop top)))))


(transaction
 (defmethod put ((s tstack) value)
   "Insert VALUE as first element in tstack S and return VALUE.
Since tstack can contain unlimited values, this method never blocks."
   (push value (_ s top))
   value))
   

(transaction
 (defmethod try-take ((s tstack))
   "If tstack S contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (with-rw-slots (top) s
     (if (null top)
         (values nil nil)
         (values t (pop top))))))
   

(defmethod try-put  ((s tstack) value)
  "Append VALUE to tstack S and return (values t VALUE).
Since fifo can contain unlimited values, this method never fails."
  (values t (put s value)))




(defprint-object (obj tstack :identity nil)
  (with-ro-slots (top) obj
    (if top
        (format t "~A" (reverse top))
        (write-string "()"))))

