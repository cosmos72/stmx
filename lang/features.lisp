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
  
  (pushnew :stmx *features*)
 
  
  (declaim (type list *feature-list*))
  (defvar *feature-list* nil)

  (defun intern-feature (f)
    (declare (type symbol f))
    (if (keywordp f)
        f
        (the keyword (intern (symbol-name f) :keyword))))

  (defun get-feature (f &optional default)
    "Return value of F in *FEATURE-LIST*, or DEFAULT if not present."
    (declare (type symbol f))
    (let ((pair (assoc (intern-feature f) *feature-list*)))
      (if pair
          (values (rest pair) t)
          (values default   nil))))

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

  (defun add-feature (f &optional (value t))
    (declare (type symbol f))
    (unless (feature? f)
      (push (cons (intern-feature f) value) *feature-list*)))

  (defun add-features (&rest list)
    (declare (type list list))
    (dolist (pair list)
      (let ((feature (if (consp pair) (first pair) pair))
            (value   (if (consp pair) (rest  pair) t)))
        (add-feature feature value))))
          
 #+lispworks ;; porting still in progress
 (add-features 'disable-optimize-slot-access)

 #+abcl
 nil ;; no special features

 #+ecl
 (add-features '(bt.lock-owner . mp::lock-owner))

 #+cmucl
 (add-features '(bt.lock-owner . mp::lock-process))

 #+ccl
 (add-features '(bt.lock-owner . ccl::%%lock-owner))

 #+sbcl
 (add-features #+compare-and-swap-vops '(atomic-ops . :sbcl)
               #+memory-barrier-vops   '(mem-rw-barriers . :sbcl)
               ;; usually, bt.lock-owner it not needed on SBCL:
               ;; the combination 'atomic-ops plus 'mem-rw-barriers
               ;; provide fast-mutex, which does not use bt.lock-owner
               '(bt.lock-owner . sb-thread::mutex-owner)))





(eval-always
  (if (all-features? 'atomic-ops 'mem-rw-barriers)
      ;; the combination 'atomic-ops plus 'mem-rw-barriers
      ;; provide fast-mutex (which does not use bt.lock-owner)
      (add-features 'fast-mutex 'mutex-owner))


  ;; on x86 and x86_64, memory read-after-read and write-after-write barriers
  ;; are NOP (well, technically except for SSE)
  ;;
  ;; Unluckily, if the underlying Lisp does know about them,
  ;; trying to implement them at application level requires
  ;; finding a way to stop the compiler from reordering assembler instructions.
  ;;
  ;; While more-or-less feasible by implementing barriers as a non-inline identity function,
  ;; on most CL implementations the result is slower than the default solution
  ;; that conses but does not require barriers.
  ;;
  ;; For this reason, "trivial" memory read and write barriers are currently disabled
  #-stmx ;; #+(or x86 x8664 x86-64 x86_64)
  (unless (feature? mem-rw-barriers)
    (add-feature 'mem-rw-barriers :trivial))


  ;; if at least memory read/write barriers are available, bt.lock-owner
  ;; can be used as concurrency-safe mutex-owner even without atomic-ops
  (when (all-features? 'mem-rw-barriers 'bt.lock-owner)
    (add-feature 'mutex-owner))

  ;; (1+ most-positive-fixnum) is a power of two?
  (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
    (add-feature 'fixnum-is-powerof2))

  ;; fixnum is large enough to count 10 million transactions
  ;; per second for at least 100 years?
  (when (>= most-positive-fixnum #x7fffffffffffff)
    (add-feature 'fixnum-is-large))

  ;; both the above two features
  (when (all-features? 'fixnum-is-large 'fixnum-is-powerof2)
    (add-feature 'fixnum-is-large-powerof2)))






