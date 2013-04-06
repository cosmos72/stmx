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

;;;; ** Transactional stack (first-in-last-out) buffer

(transactional
 (defclass stack ()
   ((top  :type list :initform nil :accessor top-of))))


(defmethod empty? ((s stack))
  (null (top-of s)))

(transaction
 (defmethod empty! ((s stack))
   (setf (top-of s) nil)
   f))

(defmethod full? ((s stack))
  "A stack is never full, so this method always returns nil."
  nil)
                  

(transaction
 (defmethod peek ((s stack) &optional default)
   "Return the first value in stack S without removing it, and t as multiple values.
Return (values DEFAULT nil) if F contains no value."
   (with-ro-slots (top) s
     (if (null top)
         (values default nil)
         (values (first top) t)))))


(transaction
 (defmethod take ((s stack))
   "Wait until stack S contains at least one value, then remove and return the first value."
   (with-rw-slots (top) s
     (if (null top)
         (retry)
         (pop top)))))


(transaction
 (defmethod put ((s stack) value)
   "Insert VALUE as first element in stack S and return VALUE.
Since stack can contain unlimited values, this method never blocks."
   (push value (top-of s))
   value))
   

(transaction
 (defmethod try-take ((s stack))
   "If stack S contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (with-rw-slots (top) s
     (if (null top)
         (values nil nil)
         (values t (pop top))))))
   

(defmethod try-put  ((s stack) value)
  "Append VALUE to stack S and return (values t VALUE).
Since fifo can contain unlimited values, this method never fails."
  (values t (put s value)))


