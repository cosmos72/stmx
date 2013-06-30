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

(eval-always
  
 (pushnew :stmx *features*)

 (declaim (type list *feature-list*))
 (defvar *feature-list* nil)

 (defun intern-feature (f)
   (declare (type symbol f))
   (if (keywordp f)
       f
       (the keyword (intern (symbol-name f) :keyword))))

 (defun get-feature (f &optional default)
   "Return value of F in *FEATURE-LIST*, or DEFAULT if not present."
   (declare (type symbol f))
   (let ((pair (assoc (intern-feature f) *feature-list*)))
     (if pair
         (values (rest pair) t)
         (values default   nil))))

 (defun feature? (f)
   "Return T if F is present in *FEATURE-LIST*"
   (declare (type symbol f))
   (when (assoc (intern-feature f) *feature-list*)
     t))

 (defun all-features? (&rest list)
   "Return T if all features from LIST are present in *FEATURE-LIST*"
   (declare (type list list))
   (loop for f in list
      always (feature? f)))

 (defun any-feature? (&rest list)
   "Return T if at least one feature from LIST is present in *FEATURE-LIST*"
   (declare (type list list))
   (loop for f in list
      thereis (feature? f)))

  (defun add-feature (f &optional (value t))
    (declare (type symbol f))
    (unless (feature? f)
      (push (cons (intern-feature f) value) *feature-list*)))

  (defun add-features (&rest list)
    (declare (type list list))
    (dolist (pair list)
      (let ((feature (if (consp pair) (first pair) pair))
            (value   (if (consp pair) (rest  pair) t)))
        (add-feature feature value)))))
