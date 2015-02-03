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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;;       UNFINISHED! DO NOT USE!          ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package :cl-user)

(defpackage #:stmx.concurrent
  (:use #:cl
        #:bordeaux-threads
        #:stmx.lang
        #:stmx
        #:stmx.util)

  (:import-from #:stmx
                #:new
                #:raw-value-of
                #:do-hash))


(in-package :stmx.concurrent)

(defmacro 1m (&rest body)
  `(time (dotimes (,(gensym) 1000000)
           ,@body)))

(defmacro 1g (&rest body)
  `(time (dotimes (,(gensym) 1000000000)
           ,@body)))






;;;; ** lock-free concurrent vector

(defvar *empty-vector* #())

(defstruct (cvector (:constructor %make-cvector))
  (data *empty-vector* :type simple-array)
  (mod-hi 0 :type sb-vm:word)
  (mod-lo 0 :type sb-vm:word))


(defun make-cvector (size &key (element-type t)
                     (initial-element  nil initial-element?)
                     (initial-contents nil initial-contents?))
  (declare (type fixnum size)
           (type symbol element-type))
  (let1 v (%make-cvector)
    (setf (cvector-data v)
          (cond
            (initial-contents?
             (make-array size :element-type element-type :initial-contents initial-contents))
            (initial-element?
             (make-array size :element-type element-type :initial-element initial-element))
            (t
             (make-array size :element-type element-type))))
    v))


(declaim (inline vref))
(defun vref (vector subscript)
  "AREF for concurrent vectors"
  (declare (type cvector vector)
           (type fixnum subscript))
  (let1 data (cvector-data vector)
    (declare (type simple-vector data))
    (aref data subscript)))



(defun setf-vref-slow-path (element vector subscript)
  (declare (ignore element vector subscript))
  (error "setf-vref-slow-path invoked"))


(declaim (inline (setf vref)))
(defun (setf vref) (element vector subscript)
  "(SETF AREF) for concurrent vectors"
  (declare (type cvector vector)
           (type fixnum subscript))
  ;;(incf (the fixnum (cvector-mod-hi vector)))
  (sb-ext:atomic-incf (cvector-mod-hi vector))
  (let1 data (cvector-data vector)
    (declare (type simple-vector data))
    (setf (aref data subscript) element))
  (sb-ext:atomic-incf (cvector-mod-lo vector))
  ;;(incf (the fixnum (cvector-mod-lo vector)))
  element)


(defun incf-vref-slow-path (vector subscript delta)
  (declare (ignore vector subscript delta))
  (error "incf-vref-slow-path invoked"))


(declaim (inline incf-vref))
(defun incf-vref (vector subscript)
  "INCF AREF for concurrent vectors"
  (declare (type cvector vector)
           (type fixnum subscript))
  ;;(incf (the fixnum (cvector-mod-hi vector)))
  ;;(sb-ext:atomic-incf (cvector-mod-hi vector))
  (let1 data (cvector-data vector)
    (declare (type simple-vector data))
    (let1 new-value
        (incf (the fixnum (aref data subscript)))
      ;;(sb-ext:atomic-incf (cvector-mod-lo vector))
      ;;(incf (the fixnum (cvector-mod-lo vector)))
      new-value)))




(defmacro 1m (&rest body)
  `(time (dotimes (,(gensym) 1000000)
           ,@body)))

(defmacro 1g (&rest body)
  `(time (dotimes (,(gensym) 1000000000)
           ,@body)))


(defun test-cvector-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-cvector 10 :element-type 'fixnum)))
    (declare (type cvector v))
    ;; 9.82 nanoseconds per (vref v i) ;; 10.33 nanoseconds with (if (< subscript (length data))
    ;; 9.85 nanoseconds per (vref v 0)
    (1g (incf (the fixnum (vref v i))))))


(defun test-cvector-incf-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-cvector 10 :element-type 'fixnum)))
    (declare (type cvector v))
    ;; 10.10 nanoseconds per (incf-vref v i)
    (1g (incf-vref v i))))


(defun test-cvector (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-cvector 10)))
    (declare (type cvector v))
    ;; 11.37 nanoseconds per (vref v i)
    ;; 11.11 nanoseconds per (vref v 0)
    (1g (incf (vref v i)))))


(defun test-simple-vector-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-array 10 :element-type 'fixnum)))
    (declare (type simple-array v))
    ;; 1.77 nanoseconds per (aref v i)
    ;; 1.52 nanoseconds per (aref v 0)
    (1g (incf (the fixnum (aref v i))))))


(defun test-simple-vector (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-array 10)))
    (declare (type simple-vector v))
    ;; 2.73 nanoseconds per (aref v i)
    ;; 2.28 nanoseconds per (aref v 0)
    (1g (incf (aref v i)))))


(defun test-vector-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-array 10 :adjustable t)))
    (declare (type (and vector (not simple-array)) v))
    ;; 13.64 nanoseconds per (aref v i)
    ;; 13.86 nanoseconds per (aref v 0)
    (1g (incf (the fixnum (aref v i))))))

(defun test-vector (&optional (i 0))
  (declare (type fixnum i))
  (let ((v (make-array 10 :adjustable t)))
    (declare (type (and vector (not simple-array)) v))
    ;; 16.72 nanoseconds per (aref v i)
    ;; 15.97 nanoseconds per (aref v 0)
    (1g (incf (aref v i)))))



(defun test-hash10-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((h (make-hash-table :test 'eql :size 10)))
    (dotimes (j 10)
      (setf (gethash (the fixnum j) h) (the fixnum 0)))
    ;; 15.44 nanoseconds per (gethash i h)
    ;; 15.08 nanoseconds per (gethash 0 h)
    (1g (incf (the fixnum (gethash i h))))))


(defun test-hash100-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((h (make-hash-table :test 'eql :size 100)))
    (dotimes (j 100)
      (setf (gethash (the fixnum j) h) (the fixnum 0)))
    ;; 15.45 nanoseconds per (gethash i h)
    ;; 15.06 nanoseconds per (gethash 0 h)
    (1g (incf (the fixnum (gethash i h))))))


(defun test-get-set-hash100-fixnum (&optional (i 0))
  (declare (type fixnum i))
  (let ((h (make-hash-table :test 'eql :size 100)))
    (dotimes (j 100)
      (set-hash h (the fixnum j) (the fixnum 0)))
    ;; 15.44 nanoseconds per (gethash i h)
    ;; 15.08 nanoseconds per (gethash 0 h)
    (1g (incf (the fixnum (gethash i h))))))


(defun test-recycle-hash (&optional (n 16))
  (declare (type fixnum n))
  (let ((h (make-hash-table :test 'eq)))
    ;;  0.672 microseconds (n = 16)
    ;;  1.383 microseconds (n = 32)
    ;;  2.763 microseconds (n = 64)
    ;;  5.502 microseconds (n = 128)
    ;; 10.940 microseconds (n = 256)
    ;; 21.905 microseconds (n = 512)
    (1m
     (dotimes (j n) (setf (gethash j h) t))
     (clrhash h))))

(defun test-recycle-hash2 (&optional (n 16))
  (declare (type fixnum n))
  (let ((h (make-hash-table :test 'eq)))
    (dotimes (j n) (setf (gethash j h) t))
    (clrhash h)
    ;;  0.663 microseconds (n = 16)
    ;;  0.790 microseconds (n = 32)
    ;;  0.789 microseconds (n = 63)
    ;;  0.974 microseconds (n = 64)
    ;;  0.979 microseconds (n = 127)
    ;;  1.339 microseconds (n = 128)
    ;;  2.062 microseconds (n = 256)
    ;;  3.496 microseconds (n = 512)
    (1m
     (dotimes (j 16) (setf (gethash j h) t))
     (clrhash h))))

(defun test-new-hash (&optional (n 16))
  (declare (type fixnum n))
  ;;  0.876 microseconds (n = 16)
  ;;  2.775 microseconds (n = 32)
  ;;  5.543 microseconds (n = 64)
  ;; 11.158 microseconds (n = 128)
  ;;        microseconds (n = 256)
  ;; 46.594 microseconds (n = 512)
  (1m
   (let ((h (make-hash-table :test 'eq)))
     (dotimes (j n) (setf (gethash j h) t)))))


(defun test-lock-unlock (&optional i)
  (declare (ignore i))
  (let ((lock (bt:make-lock)))
    ;; 30.06 nanoseconds per acquire + release
    (1g (bt:acquire-lock lock)
        (bt:release-lock lock))))


#+sbcl
(defun test-sbcl-lock-unlock (&optional i)
  (declare (ignore i))
  (let ((lock (sb-thread:make-mutex)))
    ;; 27.01 nanoseconds per get + release
    (1g (sb-thread:get-mutex lock)
        (sb-thread:release-mutex lock))))

