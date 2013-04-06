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

;;;; ** Transactional multicast channel with multiple readers (ports)

(transactional
 (defclass channel ()
   ((back  :type cons :initform (cons nil nil) :accessor back-of))
   (:documentation "Multicast channel supporting unlimited reading ports.
Values written into the channel are available to all reading ports in the same order.

References to values written into the channel are kept only as long as
one or more ports still need to read them.")))


(defmethod empty? ((c channel))
  "Channels are write-only: it is never possible to read values from them,
so assume they are always empty and return t."
  t)

(defmethod full? ((c channel))
  "Channels can contain unlimited values: they are never full, so always return nil."
  nil)

(transaction
 (defmethod put ((c channel) value)
   "Append VALUE as last element in channel C and return VALUE.
Since channel can contain unlimited values, this method never blocks."
   (with-rw-slots (back) c
     (let1 cell (cons nil nil)
       (setf (first back) value
             (rest  back) cell
             back cell)))
   value))
   
(defmethod try-put  ((c channel) value)
  "Append VALUE to channel C and return (values t VALUE).
Since channel can contain unlimited values, this method never fails."
  (values t (put c value)))





;;;; ** Transactional reading port for multicast channel

(transactional
 (defclass port ()
   ((front :type cons :accessor front-of)
    (channel :type channel
             :initform (error "missing :channel argument instantiating ~A or a subclass" 'port)
             :initarg :channel
             :reader channel-of
             :transactional nil))
   (:documentation "Reading port for a multicast channel.
Values written into the channel are available to all reading ports in the same order.")))


(defun channel-back-of (p)
  (declare (type port p))
  (back-of (channel-of p)))

(defun port-empty? (p)
  (declare (type port p))
  (eq (front-of p) (channel-back-of p)))
  

(defmethod initialize-instance :after ((p port) &key &allow-other-keys)
  "Initialize the reading port P for a multicast channel."
  (setf (front-of p) (channel-back-of p)))

(transaction
 (defmethod empty? ((p port))
   (port-empty? p)))

(transaction
 (defmethod empty! ((p port))
   (setf (front-of p) (channel-back-of p))
   p))
                  
(defmethod full? ((p port))
  "Ports are read-only: it is never possible to store values in them,
so assume they are always full and return t."
  t)


(transaction
 (defmethod peek ((p port) &optional default)
   "Return the first value in port P without removing it, and t as multiple values.
Return (values DEFAULT nil) if P contains no value."
   (if (port-empty? p)
       (values default nil)
       (values (first (front-of p)) t))))


(transaction
 (defmethod take ((p port))
   "Wait until port P contains at least one value, then remove and return the first value."
   (if (port-empty? p)
       (retry)
       (pop (front-of p)))))


(transaction
 (defmethod try-take ((p port))
   "If port P contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (if (port-empty? p)
       (values nil nil)
       (values t (pop (front-of p))))))
