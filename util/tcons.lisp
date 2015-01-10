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

;;;; ** Transactional cell holding two values. It is the STM equivalent of CONS cells.

#|
(transactional
 (defclass tcons ()
   ((first :initarg :first :accessor first-of)
    (rest  :initarg :rest  :accessor rest-of))
   (:documentation "Transactional cell holding two values. It is the STM equivalent of CONS cells.
To use TCONS cells, see the functions TCONS, TLIST, TFIRST and TREST.")))
|#

(declaim (inline %make-tcons %copy-tcons %tcons-first %tcons-rest))

(defstruct
    (tcons (:conc-name %tcons-) (:constructor %make-tcons)
           (:copier %copy-tcons))
  (first (error "missing ~S argument ~S" 'tcons 'first) :type tvar)
  (rest (error "missing ~S argument ~S" 'tcons 'rest) :type tvar))

(deftype tlist () '(or tcons null))

(declaim (ftype (function (t t) (values tcons &optional)) tcons)
         (ftype (function (#-ecl tlist #+ecl t) t) tfirst trest)
         (inline    tfirst trest)
         (notinline tcons
                    tcons-first (setf tfirst)
                    tcons-rest (setf trest)))

(defun tcons (first rest)
  "Create and return a new TCONS."
  (%make-tcons :first (tvar first) :rest (tvar rest)))

          
(defun tcons-first (cons)
  (declare (type tcons cons))
  (the (values t &optional) ($ (%tcons-first cons))))


(defun tcons-rest (cons)
  (declare (type tcons cons))
  (the (values t &optional) ($ (%tcons-rest cons))))



(defun tfirst (tlist)
  "Return the first element in a TCONS or TLIST."
  (when tlist (tcons-first tlist)))
  
(defun trest (tlist)
  "Return the rest element in a TCONS or TLIST."
  (when tlist (tcons-rest tlist)))

(defun (setf tfirst) (value cons)
  "Set VALUE as the first element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
  (declare (type tcons cons))
  (the (values t &optional)
       (setf ($ (%tcons-first cons)) value)))

(defun (setf trest) (value cons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
   (declare (type tcons cons))
   (the (values t &optional)
        (setf ($ (%tcons-rest cons)) value)))


(declaim (notinline tconsp)
         (inline tatom))

(defun tconsp (object)
  "Return true if OBJECT is a TCONS, and NIL otherwise."
  (typep object 'tcons))

(defun tatom (object)
  "Return false if OBJECT is a TCONS, and T otherwise."
  (not (tconsp object)))


(defun copy-tcons (cons)
  (declare (type tcons cons))
  (tcons (tcons-first cons) (tcons-rest cons)))


  

(defmacro tpush (value place)
  "Equivalent to PUSH, but for TCONS transactional cells.
Inserts VALUE as the first element in PLACE.
Return the modified PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (with-gensym var
      `(let* ((,var ,value)
              ,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,(first stores) (tcons ,var ,get-form)))
         ,store-form))))


(defmacro tpop (place)
  "Equivalent to POP, but for TCONS transactional cells.
Removes and returns the first element in PLACE."
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    (with-gensym var
      `(let* (,@(loop for temp in temps
                   for val in vals
                   collect `(,temp ,val))
              (,var ,get-form)
              (,(first stores) (trest ,var)))
         ,store-form
         (tfirst ,var)))))
      

(declaim (ftype (function (&rest t) (values tlist &optional)) tlist))

(defun tlist (&rest list)
  "Create and return a new TLIST, whose cells are TCONS."
  (when list
    (let* ((list #?+&rest-is-fresh-list (nreverse list)
                 #?-&rest-is-fresh-list (reverse list))
           (result nil))
      (dolist (e list result)
        (setf result (tcons e result))))))
               

(defprint-object (obj tcons :type nil :identity nil)
  (write-string "(")
  (loop for value = (tfirst obj)
     for rest = (trest obj)
     do
       (format t "~A" value)
       (unless (typep rest 'tcons)
         (unless (null rest)
           (format t " . ~A" rest))
         (return))
       (write-string " ")
       (setf obj rest))
  (write-string ")"))

