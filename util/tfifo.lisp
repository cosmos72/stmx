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
 (defclass tfifo ()
   ((front :type tcons :accessor front-of)
    (back  :type tcons :accessor back-of))))


(defmethod initialize-instance :after ((f tfifo) &key &allow-other-keys)
  "Initialize tfifo F."
  (let1 cell (tcons nil nil)
    (setf (front-of f) cell
          (back-of  f) cell)))


(defmethod full? ((f tfifo))
  "A tfifo is never full, so this method always returns nil."
  nil)

(transaction
 (defmethod empty? ((f tfifo))
   (eq (front-of f) (back-of f))))

(transaction
 (defmethod empty! ((f tfifo))
   (setf (front-of f) (back-of f))
   f))

                  

(transaction
 (defmethod peek ((f tfifo) &optional default)
   "Return the first value in tfifo F without removing it, and t as multiple values.
Return (values DEFAULT nil) if F contains no value."
   (with-ro-slots (front back) f
     (if (eq front back)
         (values default nil)
         (values (tfirst front) t)))))


(transaction
 (defmethod take ((f tfifo))
   "Wait until tfifo F contains at least one value,
then remove and return the first value."
   (with-rw-slots (front back) f
     (if (eq front back)
         (retry)
         (tpop front)))))


(transaction
 (defmethod put ((f tfifo) value)
   "Append VALUE as last element in tfifo F and return VALUE.
Since tfifo can contain unlimited values, this method never blocks."
   (with-rw-slots (back) f
     (let1 cell (tcons nil nil)
       (setf (tfirst back) value
             (trest  back) cell
             back cell)))
   value))
   

(transaction
 (defmethod try-take ((f tfifo))
   "If tfifo F contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (with-rw-slots (front back) f
     (if (eq front back)
         (values nil nil)
         (values t (tpop front))))))
   

(defmethod try-put  ((f tfifo) value)
  "Append VALUE as last element in tfifo F and return (values t VALUE).
Since tfifo can contain unlimited values, this method never fails."
  (values t (put f value)))


(defprint-object (obj tfifo :identity nil)
  (write-string "(")
  (let ((list (_ obj front))
        (end  (_ obj back)))
    (unless (eq list end)
      (loop 
         for value = (tfirst list)
         for rest = (trest list)
         do
           (when (eq rest end)
             (format t "~A" value)
             (return))
           (format t "~A " value)
           (setf list rest))))
  (write-string ")"))
