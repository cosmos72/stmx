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

 #-sbcl #-ccl #-cmucl
 (warn "Unsupported Common Lisp implementation. STMX is currently tested only on SBCL, CMUCL and CCL.")


 (defun add-feature (f)
   (declare (type keyword f))
   (pushnew f *features*))

 (add-feature :stmx)

 (dolist
     (f
       #+lispworks ;; porting still in progress
      '(:stmx-must-disable-optimize-slot-access)

      #+ccl
      nil

      #+cmucl
      nil

      #+sbcl
      nil #+never '(:stmx-have-fast-lock :stmx-have-sbcl.atomic-ops))

   (add-feature f))


 (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
   (pushnew :stmx-fixnum-is-power-of-two *features*)))


(eval-when (:compile-toplevel)
  (defvar *join-thread-tested* nil)
  
  (unless *join-thread-tested*
    (setf *join-thread-tested* t)
    (let ((x (gensym)))
      (when (eq x
                (bt:join-thread (bt:make-thread (lambda () x))))
        (pushnew :stmx-sane-bt.join-thread *features*)))))