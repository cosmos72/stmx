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
  #-(or abcl ccl cmucl ecl sbcl)
  (warn "Untested Common Lisp implementation.
STMX is currently tested only on ABCL, CCL, CMUCL, ECL and SBCL."))




(eval-always
  
  (defvar *features-map* (make-hash-table :test 'eq))

  (defun add-feature (feature &optional value)
    (declare (type keyword feature))
    (pushnew feature *features*)
    (when value
      (setf (gethash feature *features-map*) value)))

  (defun add-features (&rest list)
    (declare (type list list))
    (dolist (pair list)
      (let ((feature (if (consp pair) (first pair) pair))
            (value   (if (consp pair) (rest  pair) nil)))

        (add-feature feature value))))
          
 (defun features? (&rest list)
   (declare (type list list))
   (loop for f in list
      always (member (the keyword f) *features*)
      finally (return t)))

 (defun get-feature (feature)
   (declare (type keyword feature))
   (the symbol (gethash feature *features-map*)))

 (add-features :stmx)

 #+lispworks ;; porting still in progress
 (add-features :stmx.disable-optimize-slot-access)

 #+abcl
 nil ;; no special features

 #+ecl
 (add-features '(:stmx.have-mutex-owner . mp::lock-owner))

 #+cmucl
 (add-features '(:stmx.have-mutex-owner . mp::lock-process))

 #+ccl
 (add-features '(:stmx.have-mutex-owner . ccl::%%lock-owner))

 #+sbcl
 (add-features :stmx.have-atomic-ops :stmx.have-atomic-ops.sbcl))





(eval-always
  ;; stmx.have-atomic-ops implies stmx.have-mutex-owner
  #+stmx.have-atomic-ops
  (add-feature :stmx.have-mutex-owner)

 ;; (1+ most-positive-fixnum) is a power of two?
 (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
   (add-feature :stmx.fixnum-is-powerof2))

 ;; fixnum is large enough to count 10 million transactions
 ;; per second for at least 100 years?
 (when (>= most-positive-fixnum #x7fffffffffffff)
   (add-feature :stmx.fixnum-is-large))

 ;; both the above two features
 (when (features? :stmx.fixnum-is-large :stmx.fixnum-is-powerof2)
   (add-feature :stmx.fixnum-is-large-powerof2)))


(eval-always
 (defun compile-if (flag)
   "Flag is a generalized boolean.
If flag is NIL, return a keyword _not_ present in *features* (currently returns :never)
otherwise return a keyword present in *features* (currently returns :stmx)"
   (if flag :stmx :never))

 (defun compile-find-symbol (pkg-name symbol-name)
   "Find symbol in specified package and return it.
Return NIL if not found."
   (declare (type symbol pkg-name symbol-name))
   (when-bind pkg (find-package pkg-name)
     (find-symbol (string symbol-name) pkg)))

 (defun compile-if-find-symbol (pkg-name symbol-name)
   "Find symbol in specified package and:
if found, return a keyword present in *features* (currently returns :stmx)
otherwise return a keyword _not_ present in *features* (currently returns :never)"
   (declare (type symbol pkg-name symbol-name))
   (compile-if (compile-find-symbol pkg-name symbol-name))))





