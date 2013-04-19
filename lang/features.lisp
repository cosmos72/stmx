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


#-sbcl #-ccl #-lispworks
(eval-always
  (error "unsupported Common Lisp implementation. STMX currently supports only SBCL, CCL and LispWorks."))

(defun add-feature (f)
  (declare (type keyword f))
  (pushnew f *features*))

(add-feature :stmx)

#+lispworks
(dolist (f '(:stmx-must-disable-optimize-slot-access))
  (add-feature f))

#+ccl
(dolist (f '(:stmx-sane-bt.join-thread))
  (add-feature f *features*))

#+sbcl
(dolist (f '(:stmx-sane-bt.join-thread
             :stmx-have-mop.setf-slot-definition-type
             :stmx-have-mop.setf-slot-definition-initfunction))
  (add-feature f))
