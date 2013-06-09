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


(eval-when (:compile-toplevel)
  #-(or abcl ccl cmucl sbcl)
  (warn "Untested Common Lisp implementation.
STMX is currently tested only on ABCL, CCL, CMUCL and SBCL."))




(eval-always
 (defun add-features (&rest list)
   (declare (type list list))
   (dolist (f list)
     (pushnew (the keyword f) *features*)))

 (defun features? (&rest list)
   (declare (type list list))
   (loop for f in list
      always (member (the keyword f) *features*)
      finally (return t)))

 (add-features :stmx)

 #+lispworks ;; porting still in progress
 (add-features :stmx.disable-optimize-slot-access)

 #+(or ccl cmucl)
 nil ;; nothing to do

 #+sbcl
 (add-features :stmx.have-atomic-ops :stmx.have-atomic-ops.sbcl))





(eval-always
 ;; (1+ most-positive-fixnum) is a power of two?
 (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
   (add-features :stmx.fixnum-is-powerof2))

 ;; fixnum is large enough to count 10 million transactions
 ;; per second for at least 100 years?
 (when (>= most-positive-fixnum #x7fffffffffffff)
   (add-features :stmx.fixnum-is-large))

 ;; both the above two features
 (when (features? :stmx.fixnum-is-large :stmx.fixnum-is-powerof2)
   (add-features :stmx.fixnum-is-large-powerof2)))


(eval-always
 (defun compile-if (flag)
   (declare (type boolean flag))
   (if flag :stmx :never))

 (defun compile-find-symbol (pkg-name symbol-name)
   (declare (type symbol pkg-name symbol-name))
   (when-bind pkg (find-package pkg-name)
     (find-symbol (string symbol-name) pkg)))

 (defun compile-if-find-symbol (pkg-name symbol-name)
   (declare (type symbol pkg-name symbol-name))
   (compile-if (compile-find-symbol pkg-name symbol-name))))





(eval-when (:compile-toplevel)
  (defvar *join-thread-tested* nil)
  
  (unless *join-thread-tested*
    (setf *join-thread-tested* t)
    (let ((x (gensym)))
      (when (eq x
                (bt:join-thread (bt:make-thread (lambda () x))))
        (add-features :stmx.sane-bt.join-thread)))))
