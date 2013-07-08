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


(enable-#?-syntax)


(eval-always
  
 #+lispworks ;; incomplete porting
 (add-features 'disable-optimize-slot-access)

 #+abcl
 (add-features '(bt/lock-owner . :abcl))

 #+ecl
 (add-features '(bt/lock-owner . mp::lock-owner))

 #+cmucl
 (add-features '(bt/lock-owner . mp::lock-process))

 #+ccl
 (add-features '(bt/lock-owner . ccl::%%lock-owner))

 #+sbcl
 (add-features #+compare-and-swap-vops '(atomic-ops . :sbcl)
               #+memory-barrier-vops   '(mem-rw-barriers . :sbcl)
               ;; usually, bt/lock-owner it not needed on SBCL: the combo
               ;; ATOMIC-OPS + MEM-RW-BARRIERS provide FAST-MUTEX, which implements
               ;; its own mutex-owner, without resorting to bt/lock-owner
               '(bt/lock-owner . sb-thread::mutex-owner)

               #?+(symbol sb-ext defglobal)
               '(define-global . sb-ext:defglobal)))









(eval-always

  ;; on x86 and x86_64, memory read-after-read and write-after-write barriers
  ;; are NOP (well, technically except for SSE)
  ;;
  ;; Unluckily, if the underlying Lisp does know about them,
  ;; so there is no way to stop the compiler from reordering assembler instructions.
  ;;
  ;; Luckily, the compiler cannot reorder memory-accessing assembler instructions
  ;; with function calls, which is the only guarantee we need to use bt/lock-owner
  ;; as long as we keep TVAR value and version in a CONS.
  ;;
  ;; note that in this case the memory barrier functions/macros do NOT
  ;; stop the compiler from reordering...
  #+(or x86 x8664 x86-64 x86_64)
  (unless (feature? 'mem-rw-barriers)
    (add-feature 'mem-rw-barriers :trivial))


  (unless (eql (get-feature 'mem-rw-barriers) :trivial)
    ;; FAST-MUTEX requires atomic compare-and-swap plus real memory barriers.
    ;; Also, fast-mutex provides the preferred implementation of mutex-owner,
    ;;   which does not use bt/lock-owner
    (if (all-features? 'atomic-ops 'mem-rw-barriers)
        (add-features 'fast-mutex '(mutex-owner . :fast-mutex))))


  ;; if at least fake memory read/write barriers are available, bt/lock-owner
  ;; can be used as concurrency-safe mutex-owner even without atomic-ops
  (unless (feature? 'mutex-owner)
    (when (all-features? 'mem-rw-barriers 'bt/lock-owner)
      (add-feature 'mutex-owner :bt/lock-owner)))



  ;; hardware transactions are still EXPERIMENTAL.
  ;;
  ;; do we have memory barriers (even trivial ones) and mutex-owner?
  (when (all-features? 'mem-rw-barriers 'mutex-owner)
    ;; do we also have the sb-transaction package exposing CPU hardware transactions?
    #?+(symbol sb-transaction transaction-supported-p)
    ;; good, and does the current CPU actually support hardware transactions?
    (when (sb-transaction:transaction-supported-p) 
      ;; yes. start the turbines.
      (add-features '(hw-transactions . :sb-transaction))))




  ;; (1+ most-positive-fixnum) is a power of two?
  (when (zerop (logand most-positive-fixnum (1+ most-positive-fixnum)))
    (add-feature 'fixnum-is-powerof2))

  ;; fixnum is large enough to count 10 million transactions
  ;; per second for at least 100 years?
  (when (>= most-positive-fixnum #x7fffffffffffff)
    (add-feature 'fixnum-is-large))

  ;; both the above two features
  (when (all-features? 'fixnum-is-large 'fixnum-is-powerof2)
    (add-feature 'fixnum-is-large-powerof2))




  ;; which kind of locking shall we use for TVARs?
  ;;
  ;; The preferred choice is a single lock bit embedded in TVARs version.
  ;; It requires FAST-MUTEX (i.e. both ATOMIC-OPS and MEM-RW-BARRIERS)
  ;; and FIXNUM-IS-POWEROF2. In such case define the feature TVAR-LOCK to :BIT
  ;;
  ;; The second and much slower choice is to use mutexes; in such case
  ;; define the feature TVAR-LOCK to :MUTEX
  (if (all-features? 'fast-mutex 'fixnum-is-powerof2)
      (add-feature 'tvar-lock :bit)
      (add-feature 'tvar-lock :mutex))
  

  ;; use global-clock GV1 by default, but switch to GV5 for hardware transactions.
  ;;
  ;; Note: using the global-clock GV5 with software transactions
  ;; reduces performance by ~50% because it causes a lot of (rerun),
  ;; so it makes sense to use it only together with hardware transactions
  ;; (GV1 is not suitable for that)
  (add-feature 'global-clock 
               (if (feature? 'hw-transactions) :gv5 :gv1))






  (defmacro define-global (name value &optional (doc nil docp))
    "Define NAME as a global variable, declaring that it will have the same value
in all threads, i.e. it will not be special nor dynamically bound.

This is implemented either with a compiler-specific macro (for example
SB-EXT:DEFGLOBAL on SBCL), or as DEFVAR if no better implementation is available."
    
    (let1 define-global-impl (get-feature 'define-global 'defvar)
      `(,define-global-impl ,name ,value ,@(when docp `(,doc))))))
