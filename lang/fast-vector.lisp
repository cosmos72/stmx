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


(in-package :stmx.lang)

;;;; ** fast pushable vector

(defvar +empty-vector+ (make-array 0))

(defstruct (fast-vector (:constructor %make-fast-vector))
  (len   0 :type fixnum)
  (vec   +empty-vector+ :type simple-vector)
  (initial-element 0))




(declaim (ftype (function (fixnum t t) simple-vector)
                fast-vector-make-array))

(defun fast-vector-make-array (initial-capacity element-type initial-element)
  (declare (type fixnum initial-capacity))

  (let1 options nil
    (unless (eq t element-type)
      (setf options (list :element-type element-type)))

    (unless (eql 0 initial-element)
      (push initial-element options)
      (push :initial-element options))

    (apply #'make-array initial-capacity options)))


(declaim (ftype (function (fixnum &key (:element-type t) (:initial-element t)) fast-vector)
                make-fast-vector))

(defun make-fast-vector (initial-capacity &key (element-type t) (initial-element 0))
  "Create and return an initially empty FAST-VECTOR with specified maximum length.

Note: initial-element MUST be compatible with type element-type,
i.e. (typep initial-element element-type) must return true."

  (the fast-vector
    (%make-fast-vector :vec (fast-vector-make-array initial-capacity element-type initial-element)
                       :initial-element initial-element)))
    

(declaim (ftype (function (fast-vector) fixnum) fast-vector-length fast-vector-capacity)
         (inline
           fast-vector-length fast-vector-capacity))


(defun fast-vector-length (fast-vector)
  "Return current length of FAST-VECTOR."
  (declare (type fast-vector fast-vector))
  (the fixnum (fast-vector-len fast-vector)))


(defun fast-vector-capacity (fast-vector)
  "Return current capacity of FAST-VECTOR."
  (declare (type fast-vector fast-vector))
  (the fixnum (length (fast-vector-vec fast-vector))))



(defmacro fast-vector-pop-macro (fast-vector &optional default)
  "If FAST-VECTOR is not empty, remove its last element and return it and t as multiple values.
Otherwise evaluate DEFAULT and return (values DEFAULT NIL)."
  (with-gensyms (fvec vec pos element)
    `(let1 ,fvec ,fast-vector
       (declare (type fast-vector ,fvec))
       (let ((,vec (fast-vector-vec ,fvec))
             (,pos (fast-vector-len ,fvec)))
         (declare (type simple-vector ,vec)
                  (type fixnum ,pos))
         (if (plusp ,pos)
             (let1 ,element (fast-vector-initial-element ,fvec)
               (setf (fast-vector-len ,fvec) (the fixnum (decf ,pos)))
               (rotatef ,element (svref ,vec ,pos))
               (values ,element t))
             (values ,default nil))))))


(declaim (ftype (function (fast-vector &optional t) (values t boolean &optional)) fast-vector-pop)
         (inline fast-vector-pop))

(defun fast-vector-pop (fast-vector &optional default)
  "If FAST-VECTOR is not empty, remove its last element and return it and t as multiple values.
Otherwise return (values DEFAULT NIL)."
  (fast-vector-pop-macro fast-vector default))



(declaim (ftype (function (t fast-vector) (values (or null fixnum) &optional)) fast-vector-push)
         (inline fast-vector-push))


(defun fast-vector-push (new-element fast-vector)
  "If FAST-VECTOR is not full, append NEW-ELEMENT to FAST-VECTOR and return
the index of the pushed element.
Otherwise return NIL."
  (declare (type fast-vector fast-vector))
  (let ((vec (fast-vector-vec fast-vector))
        (pos (fast-vector-len fast-vector)))
    (declare (type simple-vector vec)
             (type fixnum pos))

    (the (or null fixnum)
      (when (< pos (length vec))
          (setf (svref vec pos) new-element
                (fast-vector-len fast-vector) (the fixnum (1+ pos)))
          pos))))



(declaim (ftype (function (t fast-vector) (values fixnum &optional)) fast-vector-push-extend))

(defun fast-vector-push-extend (new-element fast-vector)
  "If FAST-VECTOR is full, resize it. Then append NEW-ELEMENT to FAST-VECTOR and return
the index of the pushed element."
  (declare (type fast-vector fast-vector))

  (let ((vec (fast-vector-vec fast-vector))
        (pos (fast-vector-len fast-vector)))
    (declare (type simple-vector vec)
             (type fixnum pos))

    (when (= pos (length vec))
      (let* ((new-vec (fast-vector-make-array (max 8 (ash pos 1))
                                              (array-element-type vec)
                                              (fast-vector-initial-element fast-vector))))
        (dotimes (i pos)
          (setf (svref new-vec i) (svref vec i)))
        
        (setf (fast-vector-vec fast-vector) new-vec
              vec new-vec)))
    
    (setf (svref vec pos) new-element
          (fast-vector-len fast-vector) (the fixnum (1+ pos)))
    (the fixnum pos)))


(declaim (ftype (function (fast-vector) (values fast-vector &optional)) fast-vector-clear))

(defun fast-vector-clear (fast-vector)
  "Remove all elements from FAST-VECTOR. Return FAST-VECTOR."
  (declare (type fast-vector fast-vector))

  (let ((vec (fast-vector-vec fast-vector))
        (len (fast-vector-len fast-vector))
        (element (fast-vector-initial-element fast-vector)))

    (dotimes (i len)
      (setf (svref vec i) element))
    
    (setf (fast-vector-len fast-vector) 0)
    fast-vector))


(defmacro do-fast-vector ((element) fast-vector &body body)
  (with-gensyms (fvec vec len i)
    `(let1 ,fvec ,fast-vector
       (declare (type fast-vector ,fvec))
       (let ((,vec (fast-vector-vec ,fvec))
             (,len (fast-vector-len ,fvec)))
         (declare (type simple-vector ,vec)
                  (type fixnum ,len))
         (dotimes (,i ,len)
             (let1 ,element (svref ,vec ,i)
               ,@body))))))
