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
   "Return value of F in *FEATURE-LIST* and T, or (values DEFAULT NIL) if not present."
   (declare (type symbol f))
   (let ((pair (assoc (intern-feature f) *feature-list*)))
     (if pair
         (values (rest pair) t)
         (values default nil))))

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

 (defun override-feature (f &optional (value t))
   "Set feature F to VALUE, even if F is already present in *FEATURE-LIST*.
Return VALUE."
   (declare (type symbol f))
   (let* ((f (intern-feature f))
          (pair (assoc f *feature-list*)))
     (if pair
         (setf (rest pair) value)
         (push (cons f value) *feature-list*))
     value))

 (defun rem-feature (f)
   "Remove feature F from *FEATURE-LIST*.
Return T if F was present *FEATURE-LIST*, otherwise return NIL."
   (declare (type symbol f))
   (when (feature? f)
     (let1 f (intern-feature f)
       (setf *feature-list*
             (delete-if (lambda (pair) (eql f (first pair)))
                        *feature-list*))
       t)))

 (defun clear-features ()
   "Remove all features from *FEATURE-LIST*."
   (setf *feature-list* nil))

 (defun add-feature (f &optional (value t))
   "Add feature F and its VALUE into *FEATURE-LIST*, unless F is already present.
Return (values T VALUE) if F was actually inserted in *FEATURE-LIST*,
otherwise return NIL and the value already present in *FEATURE-LIST*."
   (declare (type symbol f))
   (multiple-value-bind (old-value present?) (get-feature f)
     (if present?
         (values nil old-value)
         (progn
           (push (cons (intern-feature f) value) *feature-list*)
           (values t value)))))

 (defun add-features (&rest alist)
   "Set the value of each feature in ALIST, unless the feature is already present in *FEATURE-LIST*.
Each element in ALIST must be either a dotted pair (FEATURE . VALUE) or a simple atom FEATURE.
In the latter case, the FEATURE value will default to T."
   (declare (type list alist))
   (dolist (pair alist)
     (let ((feature (if (consp pair) (first pair) pair))
           (value   (if (consp pair) (rest  pair) t)))
       (add-feature feature value)))))
