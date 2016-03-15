;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
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
  #-(or abcl ccl clisp cmucl ecl sbcl)
  (progn
    (warn "Untested Common Lisp implementation.
STMX is currently tested only on ABCL, CCL, CLISP, CMUCL, ECL and SBCL.")

    ;; unknown system - for safety, disable even features
    ;; enabled by default on known systems
    (set-features '(bt/with-lock nil)
                  '(define-constant-once nil))


    ;; feature 'use-initialize-instance-before
    ;; is faster on some Lisps, but broken on others:
    ;;
    ;; it uses :before methods on INITIALIZE-INSTANCE,
    ;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS and UPDATE-INSTANCE-FOR-CHANGED-CLASS
    ;; to put empty TVARs into objects slots, and assumes that the main methods
    ;; internally call slot-value-using-class to set the values of the TVARs
    ;; already inside the slots, *not* the slots themselves.
    ;;
    ;; If this feature is unset or NIL, a safe default will be used, that works in all cases:
    ;; main methods will be defined on INITIALIZE-INSTANCE,
    ;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS and UPDATE-INSTANCE-FOR-CHANGED-CLASS
    ;; that first invoke (call-next-method) inside (stmx::without-recording-with-show-tvars ...)
    ;; to set raw slot values, then manually fix the transactional slots by wrapping into TVARs
    ;; the values already present.
    ;;
    ;; So we selectively enable 'use-initialize-instance-before
    ;; only where known to work *and* to be faster than the safe default.
    ;;
    ;; In both cases, :after methods are not created, keeping them available for programmers.
    ))


(enable-#?-syntax)


(eval-always
  
 #+lispworks ;; incomplete porting
 (set-features '(tclass-options ((:optimize-slot-access nil)))
               '(bt/lock-owner mp:lock-owner))
 
 #+abcl
 (set-features '(bt/lock-owner :abcl)
               ;; (closer-mop:class-slots <struct>) returns a list of simple-vectors,
               ;; not of a list of slot-definitions. We would need to special-case this...
               #-(and) 'closer-mop/works-on-structs
               '(sxhash-equalp sys:psxhash))

 #+ccl
 (set-features '(bt/lock-owner ccl::%%lock-owner)
               ;; trivial mem barriers cause "bogus object" errors.
               '(define-constant-once nil) ;; causes deadlocks
               #?+(symbol ccl defstatic) '(define-global ccl:defstatic)
               'use-initialize-instance-before
               'closer-mop/works-on-structs
               '(sxhash-equalp ccl::%%equalphash))

 #+clisp
 ;; as of CLISP 2.49, threads are experimental an disabled by default,
 ;; thus STMX compiles in single-thread mode, without using bt/with-lock and mem-rw-barriers.
 ;; On the other hand, define-constant-once and use-initialize-instance-before work.
 (set-features '(bt/with-lock nil)
               'use-initialize-instance-before
               'closer-mop/works-on-structs
               ;; on CLISP, SXHASH can be used for SXHASH-EQUALP,
               ;; with performance degradation because it hashes
               ;; 1) all HASH-TABLEs to the same hash
               ;; 2) all STRUCTURE-OBJECT of the same type to the same hash
               ;; potentially causing a lot of conflicts in GHASH-TABLE and THASH-TABLE
               '(sxhash-equalp sxhash))

 #+cmucl
 (set-features '(bt/lock-owner mp::lock-process)
               'closer-mop/works-on-structs
               '(sxhash-equalp (lisp::internal-equalp-hash * 0)))
 
 #+ecl
 ;; ECL versions up to 13.5.1 have issues with reloading bordeaux-threads from cached FASLs.
 ;; as of 2015-02-07, latest ECL from git://git.code.sf.net/p/ecls/ecl seems to fix them
 (set-features '(bt/lock-owner mp::lock-owner)
               '(bt/with-lock nil) ;; bugged?
               '(define-constant-once nil) ;; bugged?
               'use-initialize-instance-before
               'closer-mop/works-on-structs
               '(sxhash-equalp si:hash-equalp))
 
 #+sbcl
 (set-features #+compare-and-swap-vops '(atomic-ops :sbcl)
               #+memory-barrier-vops '(mem-rw-barriers :sbcl)

               ;; usually, bt/lock-owner it not needed on SBCL: the combo
               ;; ATOMIC-OPS + MEM-RW-BARRIERS provides FAST-MUTEX, which implements
               ;; its own mutex-owner, without resorting to bt/lock-owner
               '(bt/lock-owner sb-thread::mutex-owner)

               #?+(symbol sb-ext defglobal) '(define-global sb-ext:defglobal)

               'use-initialize-instance-before
               'closer-mop/works-on-structs
               '(sxhash-equalp sb-impl::psxhash)))



(eval-always
  (default-feature 'define-constant-once)

  (flet ((list-args (&rest list) list))
    (let* ((x '(1 2 3 4))
           (y (apply #'list-args x)))
      (set-feature '&rest-is-fresh-list (not (eq x y)))))

  ;; (1+ most-positive-fixnum) is a power of two?
  (set-feature 'fixnum-is-powerof2
               (zerop (logand most-positive-fixnum (1+ most-positive-fixnum))))

  ;; fixnum is large enough to count 20 million transactions
  ;; per second for at least 50 years?
  (set-feature 'fixnum-is-large
               (>= most-positive-fixnum #x7fffffffffffff))

  ;; both the above two features
  (set-feature 'fixnum-is-large-powerof2
               (all-features 'fixnum-is-large 'fixnum-is-powerof2)))


              

;; fix features if no thread support
#?-bt/make-thread
(eval-always
  (set-features '(mem-rw-barriers :trivial)
		'(atomic-ops      nil)
		'(bt/with-lock    :single-thread)
		'(bt/lock-owner   :single-thread)
		'(mutex-owner     :single-thread)
		'(fast-mutex      :single-thread)
		'(tvar-lock       :single-thread)))



;; detect and compose features
(eval-always
  (default-feature 'bt/with-lock :fast)

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
  ;; (unless a better implementation is available, obviously)
  ;;
  ;; For safety, we let each implementation decide whether to set
  ;; '(mem-rw-barriers :trivial) on x86 and x86-64


  (default-feature 'mem-rw-barriers nil)


  ;; FAST-MUTEX requires atomic compare-and-swap plus *real* memory barriers.
  ;; Also, fast-mutex provides the preferred implementation of mutex-owner,
  ;;   which does not use bt/lock-owner
  (when (all-features 'atomic-ops 'mem-rw-barriers)
    (unless (eql (get-feature 'mem-rw-barriers) :trivial)
      (default-features 'fast-mutex
                        '(mutex-owner :fast-mutex))))


  ;; if mem-rw-barriers (even trivial ones) are available, bt/lock-owner
  ;; can be used as concurrency-safe mutex-owner even without atomic-ops
  (when (all-features 'mem-rw-barriers 'bt/lock-owner)
    (default-feature 'mutex-owner :bt/lock-owner))



  ;; hardware transactions need:
  ;; 1) transactional CPU instructions (currently Intel TSX)
  ;; 2) memory barriers (even trivial ones will do)
  ;; 3) mutex-owner
  ;;
  (default-feature 'hw-transactions nil)
  (when (all-features 'mem-rw-barriers 'mutex-owner)
    ;; do we also have the STMX.ASM package exposing CPU hardware transactions?
    #?+(symbol stmx.asm transaction-supported-p)
    ;; good, and does the current CPU actually support hardware transactions?
    (when (stmx.asm:transaction-supported-p) 
      ;; yes. start the turbines.
      (set-feature 'hw-transactions :stmx.asm)))



  ;; which kind of locking shall we use for TVARs?
  ;;
  ;; The preferred choice is a single lock bit embedded in TVARs version.
  ;; It requires FAST-MUTEX (i.e. both ATOMIC-OPS and MEM-RW-BARRIERS)
  ;; and FIXNUM-IS-POWEROF2. In such case define the feature TVAR-LOCK to :BIT
  ;;
  ;; The second and much slower choice is to use mutexes; in such case
  ;; define the feature TVAR-LOCK to :MUTEX
  (if (all-features 'fast-mutex 'fixnum-is-powerof2)
      (default-feature 'tvar-lock :bit)
      (default-feature 'tvar-lock :mutex))
  

  ;; atomic counters are (almost) 64 bit.
  ;; if fixnums are (almost) 64 bit and memory barriers and atomic-ops are available,
  ;; atomic counters use them and do not need locking.
  ;; otherwise, atomic counters will need locking (using mutexes)
  (set-feature 'fast-atomic-counter
               (all-features 'atomic-ops 'mem-rw-barriers 'fixnum-is-large-powerof2))



  ;; use global-clock GV1 by default, but switch to GV6 for hardware transactions.
  ;;
  ;; Note: GV6 adaptively switches between global-clock GV1 and GV5,
  ;;       with GV1 being unsuitable for hardware transactions
  ;;       and GV5 reducing performance of software transactions by ~50%
  ;;       because it causes a lot of (rerun)
  (set-feature 'global-clock 
     (if (get-feature 'hw-transactions) :gv6 :gv1))






  (defmacro define-global (name value &optional (doc nil docp))
    "Define NAME as a global variable, declaring that it will have the same value
in all threads, i.e. it will not be special nor dynamically bound.

This is implemented either with a compiler-specific macro (for example
SB-EXT:DEFGLOBAL on SBCL), or as DEFVAR if no better implementation is available."
    
    (let1 impl (get-feature 'define-global 'defvar)
      `(,impl ,name ,value ,@(when docp `(,doc)))))

  
  (defmacro define-constant-once (name value &optional (doc nil docp))
    "Same as DEFCONSTANT, but evaluate VALUE only once:
re-executing again the same (DEFINE-CONSTANT-ONCE name ...) has no effects."

    (let1 impl (get-feature 'define-constant-once 'define-global)
      (case impl
        ((t)
         `(defconstant ,name 
            (if (boundp ',name) (symbol-value ',name) ,value)
            ,@(when docp `(,doc))))
        (otherwise
          `(,impl ,name ,value ,@(when docp `(,doc))))))))



