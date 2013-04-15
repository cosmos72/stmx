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

;;;; * Printing utilities


(defgeneric id-of (obj))
(defgeneric (setf id-of) (value obj))

(defun compute-string-of (obj)
  (handler-case
      (format nil "~A" obj)
    (t ()
      (handler-case
          (format nil "~S" obj)
        (t ()
          "<error printing object>")))))

(defun compute-id-of (obj)
  (declare (type t obj))
  (let* ((str (the string (compute-string-of obj)))
         (beg (position #\{ str))
         (end (position #\} str)))
    (the string
      (if (and beg end)
          (subseq str (1+ beg) end)
          str))))

(eval-always
  (let1 ids
      #+sbcl (make-hash-table :test 'eq :size 100 :weakness :key)
      #-sbcl (make-hash-table :test 'eq :size 100 :weak :key)
      
      (defmethod id-of (obj)
        (the string
          (or
           (gethash obj ids)
           (setf (gethash obj ids) (compute-id-of obj)))))

      (defmethod (setf id-of) (value obj)
        (set-hash ids obj (format nil "~A" value)))))


(declaim (inline ~ (setf ~)))

(defun ~ (obj)
  (id-of obj))

(defun (setf ~) (value obj)
  (setf (id-of obj) value))



(defmacro defprint-object ((obj class &key (type t) (identity t)) &rest body)
  (let1 stream (gensym "STREAM-")
    `(defmethod print-object ((,obj ,class) ,stream)
       (print-unreadable-object (,obj ,stream :type ,type :identity ,identity)
         (prog ((*standard-output* ,stream))
            (handler-bind ((error
                            (lambda (err)
                              (declare (ignore err))
                              (write-string "<error printing object>")
                              (go label-defprint-object-out))))
              ,@body)
            label-defprint-object-out)))))
         
                              