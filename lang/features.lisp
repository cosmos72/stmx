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
 (warn "Untested Common Lisp implementation. STMX is currently tested only on SBCL, CMUCL and CCL.")

 (defun add-feature (f)
   (declare (type keyword f))
   (pushnew f *features*))

 (add-feature :stmx)

 (dolist
     (f
       #+lispworks ;; porting still in progress
      '(:stmx-must-disable-optimize-slot-access)

      #+sbcl
      '(:stmx-have-fast-lock :stmx-have-sbcl.atomic-ops)

      #-lispworks #-sbcl
      nil)

   (add-feature f))


 ;; (1+ most-positive-fixnum) is a power of two?
 (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
   (pushnew :stmx-fixnum-is-power-of-two *features*))


 ;; fixnum is large enough to count 10 million transactions
 ;; per second for at least 100 years?
 (when (>= most-positive-fixnum #x7fffffffffffff)
   (pushnew :stmx-fixnum-is-large *features*))

 ;; both the above two features
 (when (and (member :stmx-fixnum-is-large *features*)
            (member :stmx-fixnum-is-power-of-two *features*))
   (pushnew :stmx-fixnum-is-large-power-of-two *features*)))




(eval-when (:compile-toplevel)
  (defvar *join-thread-tested* nil)
  
  (unless *join-thread-tested*
    (setf *join-thread-tested* t)
    (let ((x (gensym)))
      (when (eq x
                (bt:join-thread (bt:make-thread (lambda () x))))
        (pushnew :stmx-sane-bt.join-thread *features*)))))
