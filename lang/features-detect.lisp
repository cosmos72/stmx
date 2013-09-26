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
 (add-features 'slot-access.disable-optimize)

 #+abcl
 (add-features '(bt/lock-owner . :abcl)
               '(bt/with-lock . :fast)
               #+(or x86 x8664 x86-64 x86_64) '(mem-rw-barriers . :trivial)
               'define-constant-once)

 #+ecl
 (add-features '(bt/lock-owner . mp::lock-owner)
               #+never 'define-constant-once) ;; bugged?

 #+cmucl
 (add-features '(bt/lock-owner . mp::lock-process))
               ;; '(bt/with-lock . :fast)
               ;; #+(or x86 x8664 x86-64 x86_64) '(mem-rw-barriers . :trivial)
               ;; 'define-constant-once

 #+ccl
 (add-features '(bt/lock-owner . ccl::%%lock-owner)
               '(bt/with-lock . :fast)
               #+(or x86 x8664 x86-64 x86_64) '(mem-rw-barriers . :trivial)
               ;; on CCL, 'define-constant-once causes test-suite to hang!
               #+never 'define-constant-once)
               

 #+sbcl
 (add-features #+compare-and-swap-vops '(atomic-ops . :sbcl)
               #+memory-barrier-vops   '(mem-rw-barriers . :sbcl)

               ;; usually not needed, SBCL has #+memory-barrier-vops
               #-memory-barrier-vops
               #+(or x86 x8664 x86-64 x86_64) '(mem-rw-barriers . :trivial)

               ;; usually, bt/lock-owner it not needed on SBCL: the combo
               ;; ATOMIC-OPS + MEM-RW-BARRIERS provides FAST-MUTEX, which implements
               ;; its own mutex-owner, without resorting to bt/lock-owner
               '(bt/lock-owner . sb-thread::mutex-owner)
               '(bt/with-lock . :fast)

               #?+(symbol sb-ext defglobal) '(define-global . sb-ext:defglobal)
               'define-constant-once))







(eval-always

  ;; on x86 and x86_64, memory read-after-read and write-after-write barriers
  ;; are NOP (well, technically except for SSE)
  ;;
  ;; Unluckily, if the underlying Lisp does not know about them,
  ;; so there is no way to stop the compiler from reordering assembler instructions.
  ;;
  ;; Luckily, the compiler cannot reorder memory-accessing assembler instructions
  ;; with function calls, which is the only guarantee we need to use bt/lock-owner
  ;; as long as we keep TVAR value and version in a CONS.
  ;;
  ;; note that in this case the memory barrier functions/macros do NOT
  ;; stop the compiler from reordering...
  ;;
  ;; Summarizing, for most Lisp compilers (ECL being a notable exception)
  ;; 'mem-rw-barriers feature can be set to :trivial on x86 and x86-64
  ;; (unless a better implementation is available, ovbviously)


  (unless (eql (get-feature 'mem-rw-barriers) :trivial)
    ;; FAST-MUTEX requires atomic compare-and-swap plus *real* memory barriers.
    ;; Also, fast-mutex provides the preferred implementation of mutex-owner,
    ;;   which does not use bt/lock-owner
    (if (all-features? 'atomic-ops 'mem-rw-barriers)
        (add-features 'fast-mutex
                      '(mutex-owner . :fast-mutex))))


  ;; if mem-rw-barriers (even trivial ones) are available, bt/lock-owner
  ;; can be used as concurrency-safe mutex-owner even without atomic-ops
  (unless (feature? 'mutex-owner)
    (when (all-features? 'mem-rw-barriers 'bt/lock-owner)
      (add-feature 'mutex-owner :bt/lock-owner)))



  ;; hardware transactions need:
  ;; 1) transactional CPU instructions (currently Intel TSX)
  ;; 2) memory barriers (even trivial ones will do)
  ;; 3) mutex-owner
  ;;
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

  ;; fixnum is large enough to count 20 million transactions
  ;; per second for at least 50 years?
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
  

  ;; atomic counters are (almost) 64 bit.
  ;; if fixnums are (almost) 64 bit and memory barriers and atomic-ops are available,
  ;; atomic counters use them and do not need locking.
  ;; otherwise, atomic counters will need locking (using mutexes)
  (when (all-features? 'atomic-ops 'mem-rw-barriers 'fixnum-is-large-powerof2)
      (add-feature 'fast-atomic-counter))



  ;; use global-clock GV1 by default, but switch to GV6 for hardware transactions.
  ;;
  ;; Note: GV6 adaptively switches between global-clock GV1 and GV5,
  ;;       with GV1 being unsuitable for hardware transactions
  ;;       and GV5 reducing performance of software transactions by ~50%
  ;;       because it causes a lot of (rerun)
  (override-feature 'global-clock 
     (if (feature? 'hw-transactions) :gv6 :gv1))






  (defmacro define-global (name value &optional (doc nil docp))
    "Define NAME as a global variable, declaring that it will have the same value
in all threads, i.e. it will not be special nor dynamically bound.

This is implemented either with a compiler-specific macro (for example
SB-EXT:DEFGLOBAL on SBCL), or as DEFVAR if no better implementation is available."
    
    (let1 impl (get-feature 'define-global 'defvar)
      `(,impl ,name ,value ,@(when docp `(,doc)))))

  
  (defmacro define-constant-once (name value &optional (doc nil docp))
    "Same as DEFCONSTANT, but evaluate VALUE only once:
re-executing again the same (DEFINE-CONSTANT-EVAL-ONCE name ...) has no effects."

    (let1 impl (get-feature 'define-constant-once 'defvar)
      (case impl
        ((t)
         `(defconstant ,name 
            (if (boundp ',name) (symbol-value ',name) ,value)
            ,@(when docp `(,doc))))
        (otherwise
          `(,impl ,name ,value ,@(when docp `(,doc))))))))



