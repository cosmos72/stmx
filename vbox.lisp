;; -*- lisp -*-

(in-package :stmx)

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




;; Copyright (c) 2013, Massimiliano Ghilardi
;; This file is part of STMX.
;;
;; STMX is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; STMX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with STMX. If not, see <http://www.gnu.org/licenses/>.
