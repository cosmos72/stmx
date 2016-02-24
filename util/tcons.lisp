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

(enable-#?-syntax)

;;;; ** TCONS: a transactional cell holding two values. It is the STM equivalent of CONS cells.


(declaim (notinline tcons-first (setf tcons-first)
                    tcons-rest  (setf tcons-rest)
                    tconsp)
         (inline    make-tcons))


;; transactional objects are a little slow...
;; use a transactional struct instead

#-(and)
(transactional
 (defclass tcons ()
   ((first :initarg :first :accessor first-of)
    (rest  :initarg :rest  :accessor rest-of))
   (:documentation "Transactional cell holding two values. It is the STM equivalent of CONS cells.
To use TCONS cells, see the functions TCONS, TLIST, TFIRST and TREST.")))


#+(and)
(transactional
 (defstruct (tcons (:predicate tconsp) (:copier copy-tcons))
   "Transactional cell holding two values. It is the STM equivalent of CONS cells.
To use TCONS cells, prepend T to the name of most list-manipulating functions. Examples:
 (CONS a b) -> (TCONS a b)
 (LIST ...) -> (TLIST ...)
 (FIRST c)  -> (TFIRST c)
 (REST  c)  -> (TREST  c) and so on"
   (first nil)
   (rest  nil)))


(deftype tlist () '(or tcons null))

(declaim (ftype (function (t t) (values tcons &optional)) tcons)
         (ftype (function (#-ecl tlist #+ecl t) t) tfirst trest)
         (notinline tcons)
         (inline    tfirst (setf tfirst)
                    trest  (setf trest)))


(defun tcons (first rest)
  "Create and return a new TCONS."
  (make-tcons :first first :rest rest))


(optimize-for-transaction*
 (:inline t)
 (defun tfirst (tlist)
   "Return the first element in a TCONS or TLIST."
   (when tlist (tcons-first tlist))))
  
(optimize-for-transaction*
 (:inline t)
 (defun trest (tlist)
   "Return the rest element in a TCONS or TLIST."
   (when tlist (tcons-rest tlist))))

(optimize-for-transaction*
 (:inline t)
 (defun (setf tfirst) (value cons)
   "Set VALUE as the first element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
   (declare (type tcons cons))
   (the (values t &optional)
        (setf (tcons-first cons) value))))

(optimize-for-transaction*
 (:inline t)
 (defun (setf trest) (value cons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
   (declare (type tcons cons))
   (the (values t &optional)
        (setf (tcons-rest cons) value))))


;; defined automatically by (defstruct tcons ...) above
#-(and)
(defun copy-tcons (cons)
  (declare (type tcons cons))
  (tcons (tcons-first cons) (tcons-rest cons)))


;; defined automatically by (defstruct tcons ...) above
#-(and)
(defun tconsp (object)
  "Return T if OBJECT is a TCONS, and NIL otherwise."
  (not (tconsp object)))

(declaim (inline tatom))

(defun tatom (object)
  "Return NIL if OBJECT is a TCONS, and T otherwise."
  (not (tconsp object)))



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



(optimize-for-transaction*
 (:inline t)
 (defun trplaca (tcons x)
   "Change the TCAR of TCONS to X and return the TCONS."
   (declare (type tcons tcons))
   (setf (tcons-first tcons) x)
   tcons))
           

(optimize-for-transaction*
 (:inline t)
 (defun trplacd (tcons x)
   "Change the TCDR of TCONS to X and return the TCONS."
   (declare (type tcons tcons))
   (setf (tcons-rest tcons) x)
   tcons))
           

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
      





(declaim (inline tlistp))

(defun tlistp (object)
  "Return true if OBJECT is a TLIST, and NIL otherwise."
  (or (null object) (tconsp object)))

       
(declaim (ftype (function (&rest t) (values tlist &optional)) tlist))

(defun tlist (&rest list)
  "Create and return a new TLIST, whose cells are TCONS."
  (when list
    (let ((list #?+&rest-is-fresh-list (nreverse list)
                #?-&rest-is-fresh-list (reverse list))
          (result nil))
      (dolist (e list result)
        (setf result (tcons e result))))))
               

(define-compiler-macro tlist (&whole form &rest list)
  (cond
    ((null list)               nil)
    ((null (rest list))        `(tcons ,(first list) nil))
    ((null (rest (rest list))) `(tcons ,(first list) (tcons ,(second list) nil)))
    (t form)))




(defun tlist* (arg0 &rest args)
  "Return a TLIST of the arguments with last TCONS a dotted pair."
  ;; We know the &REST is a proper list.
  (cond ((null args) arg0)
        ((atom (rest args)) (tcons arg0 (first args)))
        (t (let* ((args #?+&rest-is-fresh-list (nreverse args)
                        #?-&rest-is-fresh-list (reverse args))
                  (argn (pop args))
                  (list (tcons (pop args) argn)))
             (dolist (argx args)
               (setf list (tcons argx list)))
             (tcons arg0 list)))))


(define-compiler-macro tlist* (&whole form arg0 &rest args)
  (cond
    ((null args)               arg0)
    ((null (rest args))        `(tcons ,arg0 ,(first args)))
    ((null (rest (rest args))) `(tcons ,arg0 (tcons ,(first args) ,(second args))))
    (t form)))


(defun make-tlist (size &key initial-element)
  "Constructs a tlist with SIZE elements each set to INITIAL-ELEMENT"
  (declare (type fixnum size))
  (do ((count size (1- count))
       (result nil (tcons initial-element result)))
      ((<= count 0) result)
    (declare (type fixnum count))))


(defmacro do-tlist ((var tlist &optional result) &body body)
  "Analogous to DOLIST, iterates on transactional list TLIST.
On each iteration, sets VAR to the element and executes BODY inside a tagbody.
Returns RESULT. Note: when RESULT is executed, VAR is set to NIL.

An implicit block named NIL surrounds DO-TLIST, so RETURN can be used
to terminate immediately the iterations and return zero or more values."
  (with-gensyms (l start)
    `(block nil
       (let ((,l ,tlist))
         (tagbody
            ,start
            (unless (tendp ,l)
              (let ((,var (tcons-first ,l)))
                (setf ,l (tcons-rest ,l))
                (tagbody
                   ,@body))
              (go ,start))))
       (let ((,var nil))
         (declare (ignorable ,var))
         ,result))))


;;; basic tlist operations.


(optimize-for-transaction*
 (:inline t)
 (defun tcar (list)
   "Return the 1st object in a TLIST."
   (declare (type tlist list))
   (tfirst list)))

(optimize-for-transaction*
 (:inline t)
 (defun tcdr (list)
   "Return all but the first object in a TLIST."
   (declare (type tlist list))
   (trest list)))


(optimize-for-transaction*
 (:inline t)
 (defun (setf tcar) (value cons)
   "Set VALUE as the first element in a TCONS or non-null TLIST.
This function should always be executed inside an STMX atomic block."
   (declare (type tcons cons))
   (setf (tcons-first cons) value)))

(optimize-for-transaction*
 (:inline t)
 (defun (setf tcdr) (value cons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST.
This function should always be executed inside an STMX atomic block."
   (declare (type tcons cons))
   (setf (tcons-rest cons) value)))



