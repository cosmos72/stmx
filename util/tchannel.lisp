;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
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
 (defclass tchannel ()
   ((back  :type tcons :initform (tcons nil nil) :accessor back-of))
   (:documentation "Transactional multicast channel supporting unlimited reading ports.
Values written into the tchannel are available to all reading ports in the same order.

References to values written into the tchannel are kept only as long as
one or more ports still need to read them.")))


(defmethod empty? ((c tchannel))
  "Tchannels are write-only: it is never possible to read values from them,
so assume they are always empty and return t."
  t)

(defmethod full? ((c tchannel))
  "Tchannels can contain unlimited values: they are never full, so always return nil."
  nil)

(transaction
 (defmethod put ((c tchannel) value)
   "Append VALUE as last element in tchannel C and return VALUE.
Since tchannel can contain unlimited values, this method never blocks."
   (with-rw-slots (back) c
     (let1 cell (tcons nil nil)
       (setf (tfirst back) value
             (trest  back) cell
             back cell)))
   value))

(defmethod try-put  ((c tchannel) value)
  "Append VALUE to tchannel C and return (values t VALUE).
Since tchannel can contain unlimited values, this method never fails."
  (values t (put c value)))





;;;; ** Transactional reading port for multicast tchannel

(transactional
 (defclass tport ()
   ((front :type cons :accessor front-of)
    (channel :type tchannel
             :initform (error "missing :channel argument instantiating ~A or a subclass" 'tport)
             :initarg :channel
             :reader channel-of
             :transactional nil))
   (:documentation "Transactional reading port for a multicast tchannel.
Values written into the tchannel are available to all reading ports in the same order.")))


(defun tchannel-back-of (p)
  (declare (type tport p))
  (_ (_ p channel) back))

(defun tport-empty? (p)
  (declare (type tport p))
  (eq (_ p front) (tchannel-back-of p)))


(defmethod initialize-instance :after ((p tport) &key &allow-other-keys)
  "Initialize the reading tport P for a multicast tchannel."
  (setf (_ p front) (tchannel-back-of p)))

(transaction
 (defmethod empty? ((p tport))
   (tport-empty? p)))

(transaction
 (defmethod empty! ((p tport))
   (setf (_ p front) (tchannel-back-of p))
   p))

(defmethod full? ((p tport))
  "Tports are read-only: it is never possible to store values in them,
so assume they are always full and return t."
  t)


(transaction
 (defmethod peek ((p tport) &optional default)
   "Return the first value in tport P without removing it, and t as multiple values.
Return (values DEFAULT nil) if P contains no value."
   (if (tport-empty? p)
       (values default nil)
       (values (tfirst (_ p front)) t))))


(transaction
 (defmethod take ((p tport))
   "Wait until tport P contains at least one value,
then remove and return the first value."
   (if (tport-empty? p)
       (retry)
       (tpop (_ p front)))))


(transaction
 (defmethod try-take ((p tport))
   "If tport P contains at least one value, remove the first value
and return t and the first value as multiple values.
Otherwise return (values nil nil)"
   (if (tport-empty? p)
       (values nil nil)
       (values t (tpop (_ p front))))))
