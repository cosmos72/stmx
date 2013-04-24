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


#-sbcl #-ccl #-cmucl
(eval-always
  (warn "Unsupported Common Lisp implementation. STMX is currently tested only on SBCL and CCL."))

(defun add-feature (f)
  (declare (type keyword f))
  (pushnew f *features*))

(add-feature :stmx)

#+lispworks ;; porting still in progress
(dolist (f '(:stmx-must-disable-optimize-slot-access))
  (add-feature f))

#+ccl
(dolist (f '())
  (add-feature f))

#+sbcl
(dolist (f '(:stmx-have-fast-lock))
  (add-feature f))


(eval-when (:compile-toplevel)
  (let ((x (gensym)))
    (when (eq x
              (bt:join-thread (bt:make-thread (lambda () x))))
      (pushnew :stmx-sane-bt.join-thread *features*))))