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

;;;; ** Transactional first-in-first-out (fifo) buffer

(transactional
 (defclass fifo ()
   ((front :type cons :accessor front-of)
    (back  :type cons :accessor back-of))))


(defmethod initialize-instance :after ((f fifo) &key &allow-other-keys)
  "Initialize a new transactional fifo."
  (let1 cell (cons nil nil)
    (setf (front-of f) cell
          (back-of  f) cell)))


(defmethod full? ((f fifo))
  "A fifo is never full, so this method always returns nil."
  nil)

(transaction
 (defmethod empty? ((f fifo))
   (eq (front-of f) (back-of f))))

(transaction
 (defmethod empty! ((f fifo))
   (setf (front-of f) (back-of f))
   f))

                  

(transaction
 (defmethod peek ((f fifo) &optional default)
   "Return the first value in fifo F without removing it, and t as multiple values.
Return (values DEFAULT nil) if F contains no value."
   (with-ro-slots (front back) f
     (if (eq front back)
         (values default nil)
         (values (first front) t)))))


(transaction
 (defmethod take ((f fifo))
   "Wait until fifo F contains at least one value, then remove and return the first value."
   (with-rw-slots (front back) f
     (if (eq front back)
         (retry)
         (pop front)))))


(transaction
 (defmethod put ((f fifo) value)
   "Append VALUE as last element in fifo F and return VALUE.
Since fifo can contain unlimited values, this method never blocks."
   (with-rw-slots (back) f
     (let1 cell (cons nil nil)
       (setf (first back) value
             (rest  back) cell
             back cell)))
   value))
   

(transaction
 (defmethod try-take ((f fifo))
   "If fifo F contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (with-rw-slots (front back) f
     (if (eq front back)
         (values nil nil)
         (values t (pop front))))))
   

(defmethod try-put  ((f fifo) value)
  "Append VALUE to fifo F and return (values t VALUE).
Since fifo can contain unlimited values, this method never fails."
  (values t (put f value)))


