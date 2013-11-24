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


(in-package :stmx)

(enable-#?-syntax)

;;;; ** Validating


(defun valid? (log)
  "Return t if LOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction."
  (declare (type tlog log))

  (log.trace "Tlog ~A valid?.." (~ log))
  (do-txhash (var val) (tlog-reads log)
    (if (eq val (raw-value-of var))
        (log.trace "Tlog ~A tvar ~A is up-to-date" (~ log) (~ var))
        (progn
          (log.trace "Tlog ~A conflict for tvar ~A: expecting ~A, found ~A"
                     (~ log) (~ var) val (raw-value-of var))
          (log.debug "Tlog ~A ..not valid" (~ log))
          (return-from valid? nil))))
  (log.trace "Tlog ~A ..is valid" (~ log))
  t)



(defun valid-and-own-or-unlocked? (log)
  "Return t if LOG is valid and unlocked, i.e. it contains an up-to-date view
of TVARs that were read during the transaction and none of them is locked
by **other** threads."
  (declare (type tlog log))

  (log.trace "Tlog ~A valid-and-own-or-unlocked?.." (~ log))
  (do-txhash (var val) (tlog-reads log)
    (if (tvar-valid-and-own-or-unlocked? var val log)

        (log.trace "Tlog ~A tvar ~A is up-to-date and unlocked" (~ log) (~ var))
        
        (progn
          (log.debug "Tlog ~A tvar ~A conflict or locked: not valid" (~ log) (~ var))
          (return-from valid-and-own-or-unlocked? nil))))

  (log.trace "Tlog ~A ..is valid and own-or-unlocked" (~ log))
  t)


(defun valid-and-unlocked? (log)
  "Return t if LOG is valid and unlocked, i.e. it contains an up-to-date view
of TVARs that were read during the transaction and none of them is locked,
not even by the current thread."
  (declare (type tlog log))

  (log.trace "Tlog ~A valid-and-unlocked?.." (~ log))
  (do-txhash (var val) (tlog-reads log)
    (if (tvar-valid-and-unlocked? var val log)

        (log.trace "Tlog ~A tvar ~A is up-to-date and unlocked" (~ log) (~ var))
        
        (progn
          (log.debug "Tlog ~A tvar ~A conflict or locked: not valid" (~ log) (~ var))
          (return-from valid-and-unlocked? nil))))

  (log.trace "Tlog ~A ..is valid and own-or-unlocked" (~ log))
  t)

#?+hw-transactions
(declaim (inline valid-hw-assisted?))

#?+hw-transactions
(defun valid-hw-assisted? (log)
  "Return t if LOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction. Does not check for locked
TVARs or version mismatches, since it is unnecessary."
  (declare (type tlog log))
  (do-txhash (var val) (tlog-reads log)
    (unless (eq val (tvar-value var))
      (return-from valid-hw-assisted? nil)))

  t)



(declaim (inline invalid? shallow-valid? shallow-invalid?))

(defun invalid? (log)
  "Return (not (valid? LOG))."
  (declare (type tlog log))
  (not (valid? log)))




(defun shallow-valid? (log)
  "Return T if a TLOG is valid. Similar to (valid? log),
but does *not* check log parents for validity."
  (declare (type tlog log))
  ;; current implementation always performs a deep validation
  (valid? log))
  

(declaim (inline shallow-invalid?))
(defun shallow-invalid? (log)
  "Return (not (shallow-valid? LOG))."
  (declare (type tlog log))
  (not (shallow-valid? log)))


(declaim (ftype (function (tvar tvar) (values boolean &optional)) tvar>))

(defun tvar> (var1 var2)
  (declare (type tvar var1 var2))
  "Compare var1 and var2 with respect to age: newer tvars usually have larger
tvar-id and are considered \"larger\". Returns (> (tvar-id var1) (tvar-id var2))."
  (> (the fixnum (tvar-id var1))
     (the fixnum (tvar-id var2)))
  #+never
  (< (the fixnum (sb-impl::get-lisp-obj-address var1))
     (the fixnum (sb-impl::get-lisp-obj-address var2)))
  #+never
  (< (the fixnum (sxhash var1))
     (the fixnum (sxhash var2))))




(declaim (inline try-lock-tvars unlock-tvars))


(defun try-lock-tvars (writes locked log)
  "Optionally sort WRITES in (tvar> ...) order,
then non-blocking acquire their locks in such order.
Reason: acquiring in unspecified order may cause livelock, as two transactions
may repeatedly try acquiring the same two TVARs in opposite order.

Return T if all VARS were locked successfully.
After return, LOCKED will contain all TXPAIR entries of locked VARS."
  (declare (type txhash-table writes)
           (type txfifo locked)
           (type tlog log))

  ;; (sort writes #'tvar>))

  (let1 reads (tlog-reads log)
    (do-txhash-entries (entry) writes
      (let ((var (the tvar (txpair-key entry)))
            (write-value   (txpair-value entry)))
        (multiple-value-bind (read-value read?) (get-txhash reads var)
          ;; degrade "write the same value previously read" to "read"
          (unless (and read? (eq read-value write-value))
            (if (try-lock-tvar var)
                (put-txfifo locked entry)
                (return-from try-lock-tvars nil)))))))
  t)



(defun unlock-tvars (locked)
  "Release vars in LOCKED in same order of acquisition."
  (declare (type txfifo locked))

  (do-txfifo (var) locked
    (unlock-tvar var)))




;;;; ** Committing


(defun ensure-tlog-before-commit (log)
  "Create tlog-before-commit log if nil, and return it."
  (declare (type tlog log))
  (the tlog-func-vector
    (or (tlog-before-commit log)
        (setf (tlog-before-commit log)
              (make-array 1 :element-type 'function :fill-pointer 0 :adjustable t)))))

(defun ensure-tlog-after-commit (log)
  "Create tlog-after-commit log if nil, and return it."
  (declare (type tlog log))
  (the tlog-func-vector
    (or (tlog-after-commit log)
        (setf (tlog-after-commit log)
              (make-array 1 :element-type 'function :fill-pointer 0 :adjustable t)))))



(defun call-before-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked immediately before the current transaction commits.

IMPORTANT: See BEFORE-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))

  #?+hw-transactions
  (when (hw-transaction-supported-and-running?)
    ;; hw transactions do not support before-commit
    (hw-transaction-abort))

  (vector-push-extend func (ensure-tlog-before-commit log))
  func)

(defun call-after-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked after the current transaction commits.

IMPORTANT: See AFTER-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))

  #?+hw-transactions
  (when (hw-transaction-supported-and-running?)
    ;; hw transactions do not support after-commit
    (hw-transaction-abort))

  (vector-push-extend func (ensure-tlog-after-commit log))
  func)



(defmacro before-commit (&body body)
  "Register BODY to be invoked immediately before the current transaction commits.
If BODY signals an error when executed, the error is propagated to the caller,
further code registered with BEFORE-COMMIT are not executed,
and the transaction rollbacks.

BODY can read and write normally to transactional memory, and in case of conflicts
the whole transaction (not only the code registered with before-commit)
is re-executed from the beginning.

WARNING: BODY cannot (retry) - attempts to do so will signal an error.
Starting a nested transaction and retrying inside that is acceptable,
as long as the (retry) does not propagate outside BODY."
  `(call-before-commit (lambda () ,@body)))


(defmacro after-commit (&body body)
  "Register BODY to be invoked after the current transaction commits.
If BODY signals an error when executed, the error is propagated
to the caller and further code registered with AFTER-COMMIT is not executed,
but the transaction remains committed.

WARNING: Code registered with after-commit has a number or restrictions:

1) BODY must not write to *any* transactional memory: the consequences
are undefined.

2) BODY can only read from transactional memory already read or written
during the same transaction. Reading from other transactional memory
has undefined consequences.

3) BODY cannot (retry) - attempts to do so will signal an error.
Starting a nested transaction and retrying inside that is acceptable
as long as the (retry) does not propagate outside BODY."
  `(call-after-commit (lambda () ,@body)))



(defun loop-funcall-on-appendable-vector (funcs)
  "Call each function in FUNCS vector. Take care that functions being invoked
can register other functions - or themselves again - with (before-commit ...)
or with (after-commit ...).
This means new elements can be appended to FUNCS vector during the loop
=> (loop for func across funcs ...) is not enough."
  (declare (type tlog-func-vector funcs))
  (loop for i from 0
     while (< i (length funcs))
     do
       (funcall (the function (aref funcs i)))))


(defun invoke-before-commit (log)
  "Before committing, call in order all functions registered
with (before-commit)
If any of them signals an error, the transaction will rollback
and the error will be propagated to the caller"
  (declare (type tlog log))
  (when-bind funcs (tlog-before-commit log)
    ;; restore recording and log as the current tlog, functions may need them
    ;; to read and write transactional memory
    (with-recording-to-tlog log
      (handler-case
          (loop-funcall-on-appendable-vector funcs)
        (rerun-error ()
          (log.trace "Tlog ~A before-commit wants to rerun" (~ log))
          (return-from invoke-before-commit nil)))))
  t)



(declaim (inline invoke-after-commit))
(defun invoke-after-commit (log)
  "After committing, call in order all functions registered with (after-commit)
If any of them signals an error, it will be propagated to the caller
but the TLOG will remain committed."
  (declare (type tlog log))
  (when-bind funcs (tlog-after-commit log)
    ;; do NOT restore recording and log as the current tlog,
    ;; AFTER-COMMIT functions run outside any transaction
    ;; (with-recording-to-tlog log
    (loop-funcall-on-appendable-vector funcs)))



#?+hw-transactions
(declaim (inline commit-hw-assisted))

#?+hw-transactions
(defun commit-hw-assisted (log changed)
  "Try to perform COMMIT using a hardware memory transaction.
Return T if successful, NIL if transaction is invalid,
or :fail if hardware memory transaction does not complete.
After successful return, CHANGED contains the list of the TVARs actually written.

Note: invokes HW-ATOMIC2, which in turn invokes GLOBAL-CLOCK/HW/STAT-{COMMITTED,ABORTED}"
  (declare (type tlog log)
           (type txfifo changed))

  (the (member t nil :fail)
    (hw-atomic2 (write-version :test-for-running-tx? nil :update-stat :swtx)

      (block nil
        ;; hardware transaction. if you need to jump out of it
        ;; use (return ...), or it will abort and roll back!
        (unless (valid-hw-assisted? log)
          (return nil))

        (do-txhash-entries (entry) (tlog-writes log)
          (let ((var (the tvar (txpair-key entry)))
                (val           (txpair-value entry)))

            ;; degrade "write the same value already present" to "read"
            ;; the hardware transaction guarantees to abort if the tvar changes value
            (unless (eq val (raw-value-of var))
              (put-txfifo changed entry)
              (setf ($-hwtx var write-version) val))))
        t)

      ;; fallback
      :fail)))

    

    
    



  



  


(declaim (inline commit-sw-update-tvars))

(defun commit-sw-update-tvars (locked log)
  "Actually COMMIT, i.e. write new values into TVARs. Also unlock all TVARs."
  (declare (type txfifo locked)
           (type tlog log)
           (ignorable log))

  (let1 write-version (the version-type
                        (global-clock/sw/start-write (tlog-read-version log)))

    (do-filter-txfifo (var val) locked
      (if (eq val (raw-value-of var))
          (progn
            ;; TVAR-LOCK is :BIT? then we need to manually unlock
            #?+(eql tvar-lock :bit)
            (unlock-tvar var)
            (rem-current-txfifo-entry))
          
          (progn
            ;; if TVAR-LOCK is :BIT?, this also unlocks VAR.
            (set-tvar-value-and-version var val
                                        (global-clock/sw/write write-version))
                   
            (log.trace "Tlog ~A tvar ~A changed value from ~A to ~A"
                       (~ log) (~ var) (raw-value-of var) val)))

      ;; TVAR-LOCK is not :BIT? then unlock manually in all cases
      #?-(eql tvar-lock :bit)
      (unlock-tvar var))))


(defun commit (log)
  "Commit a TLOG to memory.

It returns a boolean specifying whether or not the transaction
log was committed.  If the transaction log cannot be committed
it either means that:
a) the TLOG is invalid - then the whole transaction must be re-executed
b) another TLOG is writing the same TVARs being committed
   so that TVARs locks could not be aquired - also in this case
   the whole transaction will be re-executed, as there is little hope
   that the TLOG will still be valid.

Note: internally invokes GLOBAL-CLOCK/{HW,SW}/STAT-{COMMITTED,ABORTED}"
   
  (declare (type tlog log))

  ;; before-commit functions run without locks.
  ;; WARNING: they may access transactional memory,
  ;;          modifying tlog reads and writes!
  (unless (invoke-before-commit log)
    (return-from commit nil))


  (let* ((writes     (the txhash-table (tlog-writes log)))
         (locked     (the txfifo       (tlog-locked log))))
         
    (when (zerop (txhash-table-count writes))
      (log.debug "Tlog ~A committed (nothing to write)" (~ log))
      (global-clock/sw/stat-committed)
      (invoke-after-commit log)
      (return-from commit t))

    (prog ((success))
       (declare (type boolean success))

       #?+hw-transactions
       (let1 committed (commit-hw-assisted log locked)
         (log.debug "(commit-hw-assisted) returned ~S" committed)
         (case committed
           ((t)   (setf success t) (go committed))
           ((nil) (return nil))))
             

       ;; HW-assisted commit failed, but transaction may still be valid.
       ;; fall back on SW commit
       #?+hw-transactions (global-clock/incf-nohw-counter)

       (unwind-protect
            (progn
              ;; we must lock TVARs that will been written: expensive
              ;; but needed to ensure concurrent commits do not conflict.
              (log.trace "before (try-lock-tvars)")

              (unless (try-lock-tvars writes locked log)

                (log.debug "Tlog ~A failed to lock tvars, not committed" (~ log))
                (return))
            
              (log.trace "Tlog ~A acquired locks..." (~ log))

              ;; check for log validity one last time, with locks held.
              ;; Also ensure that TVARs in (tlog-reads log) are not locked
              ;; by other threads. For the reason, see doc/consistent-reads.md
              (unless (valid-and-own-or-unlocked? log)

                (log.debug "Tlog ~A is invalid or reads are locked, not committed" (~ log))
                (return))

              (log.trace "Tlog ~A committing..." (~ log))

              ;; COMMIT, i.e. actually write new values into TVARs.
              ;; Also unlock all TVARs.

              (commit-sw-update-tvars locked log)
              (global-clock/sw/stat-committed)
              (setf success t))

         ;; cleanup
         #?+hw-transactions (global-clock/decf-nohw-counter)
         
         (unless success
           (unlock-tvars locked)
           (log.trace "Tlog ~A ...released locks" (~ log))
           (global-clock/sw/stat-aborted)
           (return-from commit success)))

       
       committed

       (log.debug "Tlog ~A ...committed (and released locks)" (~ log))

       (do-txfifo (var) locked
         (log.trace "Tlog ~A notifying threads waiting on tvar ~A"
                    (~ log) (~ var))
         (notify-tvar-high-load var))

       ;; after-commit functions run without locks
       (invoke-after-commit log)
       (return-from commit success))))
                   





                   



;;;; ** Merging


(defun merge-tlog-reads (log1 log2)
  "Merge (tlog-reads LOG1) and (tlog-reads LOG2).

Return merged TLOG (either LOG1 or LOG2) if tlog-reads LOG1 and LOG2
are compatible, i.e. if they contain the same values for the TVARs
common to both, otherwise return NIL.
\(in the latter case, the merge will not be completed).

Destructively modifies (tlog-reads log1) and (tlog-reads log2)."
  (declare (type tlog log1 log2))
  (let* ((reads1 (tlog-reads log1))
         (reads2 (tlog-reads log2))
         (n1 (txhash-table-count reads1))
         (n2 (txhash-table-count reads2)))
         
    (when (< n1 n2)
      (rotatef log1 log2)
      (rotatef reads1 reads2)
      (rotatef n1 n2)) ;; guarantees n1 >= n2

    (if (or (zerop n2) (merge-txhash-tables reads1 reads2))
        log1
        nil)))

  


(defun commit-nested (log)
  "Commit LOG into its parent log; return LOG.

Unlike (commit log), this function is guaranteed to always succeed.

Implementation note: copy tlog-reads, tlog-writes, tlog-before-commit
and tlog-after-commit into parent, or swap them with parent"

  (declare (type tlog log))
  (let1 parent (the tlog (tlog-parent log))

    (rotatef (tlog-reads parent) (tlog-reads log))
    (rotatef (tlog-writes parent) (tlog-writes log))

    (when-bind funcs (tlog-before-commit log)
      (if-bind parent-funcs (tlog-before-commit parent)
        (loop for func across funcs do
             (vector-push-extend func parent-funcs))
        (rotatef (tlog-before-commit log) (tlog-before-commit parent))))

    (when-bind funcs (tlog-after-commit log)
      (if-bind parent-funcs (tlog-after-commit parent)
        (loop for func across funcs do
             (vector-push-extend func parent-funcs))
        (rotatef (tlog-after-commit log) (tlog-after-commit parent))))

    log))

