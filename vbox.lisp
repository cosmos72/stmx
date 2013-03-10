;; -*- lisp -*-

(in-package :cl-stm2)

(declaim (inline new-vbox vbox-version vbox-value vbox-bound?))

;;;; ** Versioned boxes

(defvar +unbound+ (gensym "UNBOUND-"))

(deftype vbox () 'cons)

(defun new-vbox (&key (version 0) (value +unbound+))
  "Create and return a new versioned box"
  (declare (type integer version))
  (cons version value))


;;;; ** Readers

(defun vbox-version (box)
  "Return the version stored in the versioned box"
  (declare (type cons box))
  (the integer (car box)))

(defun vbox-value (box)
  "Return the value stored in the versioned box"
  (declare (type cons box))
  (cdr box))

(defun vbox-bound? (box)
  "Return true if the versioned box is bound to a value."
  (declare (type cons box))
  (not (eq (vbox-value box) +unbound+)))

