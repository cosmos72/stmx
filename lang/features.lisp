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

 (defun assoc-feature (f)
   "Return (list F VALUE) if F is present in *FEATURE-LIST*"
   (declare (type symbol f))
   (assoc (intern-feature f) *feature-list*))

 (defun get-feature (f &optional default)
   "Return value of F in *FEATURE-LIST* and T, or (values DEFAULT NIL) if not present
or has NIL value."
   (declare (type symbol f))
   (let ((value (second (assoc-feature f))))
     (if value
         (values value   t)
         (values default nil))))

 (defun all-features (&rest list)
   "Return T if all features from LIST are present in *FEATURE-LIST*
and have non-NIL value."
   (declare (type list list))
   (loop for f in list
      always (get-feature f)))

 (defun some-features (&rest list)
   "Return T if at least one feature from LIST is present in *FEATURE-LIST*
and have non-NIL value."
   (declare (type list list))
   (loop for f in list
      thereis (get-feature f)))

 (defun rem-feature (f)
   "Remove feature F from *FEATURE-LIST*.
Return T if F was present *FEATURE-LIST*, otherwise return NIL."
   (declare (type symbol f))
   (when (assoc-feature f)
     (let1 f (intern-feature f)
       (setf *feature-list*
             (delete-if (lambda (pair) (eql f (first pair)))
                        *feature-list*))
       t)))

 (defun clear-features ()
   "Remove all features from *FEATURE-LIST*."
   (setf *feature-list* nil))

 (defun default-feature (f &optional (value t))
   "Add feature F and its VALUE into *FEATURE-LIST*, unless F is already present.
Return (values T VALUE) if F was actually inserted in *FEATURE-LIST*,
otherwise return NIL and the value already present in *FEATURE-LIST*."
   (declare (type symbol f))
   (let ((v (assoc-feature f)))
     (if v
         (values nil (rest v))
         (progn
           (push (list (intern-feature f) value) *feature-list*)
           (values t value)))))

 (defun default-features (&rest alist)
   "Set the value of each feature in ALIST, unless the feature is already
present in *FEATURE-LIST*. Each element in ALIST must be either
a pair (FEATURE VALUE) or a simple atom FEATURE.
In the latter case, the FEATURE value will default to T."
   (declare (type list alist))
   (dolist (pair alist)
     (let ((feature (if (consp pair) (first  pair) pair))
           (value   (if (consp pair) (second pair) t)))
       (default-feature feature value))))


 (defun set-feature (f &optional (value t))
   "Set feature F to VALUE, even if F is already present in *FEATURE-LIST*.
Return VALUE."
   (declare (type symbol f))
   (let* ((f (intern-feature f))
          (pair (assoc f *feature-list*)))
     (if pair
         (setf (second pair) value)
         (push (list f value) *feature-list*))
     value))


 (defun set-features (&rest plist)
   "Set the value of each feature in PLIST, even if the feature is already
present in *FEATURE-LIST*. Each element in PLIST must be either
a pair (FEATURE VALUE) or a simple atom FEATURE.
In the latter case, the FEATURE value will default to T."
   (declare (type list plist))
   (dolist (pair plist)
     (let ((feature (if (consp pair) (first pair) pair))
           (value   (if (consp pair) (second  pair) t)))
       (set-feature feature value)))))
