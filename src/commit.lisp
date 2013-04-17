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

;;;; ** Validating



#+tx-hash
(defun valid? (log)
  "Return t if a TLOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction."

  (declare (type tlog log))
  (log:trace "Tlog ~A valid?.." (~ log))
  (do-hash (var val) (tlog-reads log)
    (let1 actual-val (raw-value-of var)
      (if (eq val actual-val)
          (log:trace "Tlog ~A tvar ~A is up-to-date" (~ log) (~ var))
          (progn
            (log:trace "Tlog ~A conflict for tvar ~A: expecting ~A, found ~A"
                       (~ log) (~ var) (~ val) (~ actual-val))
            (log:debug "Tlog ~A ..not valid" (~ log))
            (return-from valid? nil)))))
  (log:trace "Tlog ~A ..is valid" (~ log))
  (return-from valid? t))

#-tx-hash
(defun valid? (log)
  "Return t if a TLOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction."

  (declare (type tlog log))
  (log:trace "Tlog ~A valid?.." (~ log))

  (let1 subscript (tlog-id log)
    (dolist (var (tlog-reads log))
      (let ((actual-val (raw-value-of var))
            (val (svref (tvar-tx-reads var) subscript)))

        (if (eq val actual-val)
            (log:trace "Tlog ~A tvar ~A is up-to-date" (~ log) (~ var))
            (progn
              (log:trace "Tlog ~A conflict for tvar ~A: expecting ~A, found ~A"
                         (~ log) (~ var) val actual-val)
              (log:debug "Tlog ~A ..not valid" (~ log))
              (return-from valid? nil)))))
    (log:trace "Tlog ~A ..is valid" (~ log))
    (return-from valid? t)))


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




(declaim (inline try-lock-tvar))
(defun try-lock-tvar (var log txt)
  "Try to acquire VAR lock non-blocking. Return t if acquired, else return nil."
  (declare (type tvar var)
           (type tlog log)
           (type string txt)
           (ignorable log txt))
  (let1 flag (acquire-lock (tvar-lock var) nil)
    #+never
    (if flag
        (log:trace "Tlog ~A locked tvar ~A" (~ log) (~ var))
        (log:debug "Tlog ~A not ~A: could not lock tvar ~A"
                   (~ log) txt (~ var)))
    flag))



(defun try-lock-tvars (vars locked-vars n-vars log txt)
  "Sort VARS in order - actually in (tvar< ...) order -
then non-blocking acquire their locks in such order.
Reason: acquiring in unspecified order may cause livelock, as two transactions
may repeatedly try acquiring the same two TVARs in opposite order.

LOCKED-VARS must be the one-element list '(nil).
Destructively modifies VARS and LOCKED-VARS.

Return t if all VARS where locked successfully, otherwise return nil.
In both cases, after this call (rest LOCKED-VARS) will be the list
containing the locked tvars, sorted in order from first acquired
to last acquired."
  (declare (type list vars locked-vars)
           (type fixnum n-vars)
           (type tlog log)
           (type string txt))

  #+never (log:user5 "unsorted TVARs to lock: (~{~A~^ ~})" vars)

  ;; if locking 1000 TVARs or more, sort them to avoid livelock.
  ;; with less than 1000 TVARS, the probability of a context switch
  ;; while locking is so low that sorting is not worth the cost
  (when (> n-vars 1000)
    (setf vars (sort vars #'tvar>)))
  #+never (log:user5 "  sorted TVARs to lock: (~{~A~^ ~})" vars)

  (loop for cell = vars then rest
     while cell
     for rest = (rest cell)
     always (try-lock-tvar (first cell) log txt)
     do (setf (rest locked-vars) cell)
       (setf locked-vars cell)
       (setf (rest cell) nil)
     finally (return t)))


(defun unlock-tvars (vars log)
  "Release locked (rest VARS) in same order of acquisition."
  (declare (type list vars)
           (type tlog log)
           (ignorable log))
  (setf vars (rest vars))
  (loop for var in vars do
       (release-lock (tvar-lock var))
       #+never (log:trace "Tlog ~A unlocked tvar ~A" (~ log) (~ var))))



#+tx-hash
(defun locked-valid? (log)
  "Return T if LOG is valid and NIL if it's invalid.
Return :UNKNOWN if relevant locks could not be acquired."

  (declare (type tlog log))
  (let* ((reads (tlog-reads log))
         (acquiring nil)
         (acquired (list nil))
         (n-reads (hash-table-count reads)))
    (declare (type fixnum n-reads))

    (when (zerop n-reads)
      (return-from locked-valid? t))

    (unwind-protect
         (progn
           ;; we must lock TVARs that have been read: expensive,
           ;; but needed to ensure this threads sees other commits as atomic
           (setf acquiring (hash-table-keys reads))

           (unless (try-lock-tvars acquiring acquired n-reads log "rolled back")
             (return-from locked-valid? :unknown))

           (let1 valid (valid? log)
             (log:debug "Tlog ~A is ~A, verified with locks"
                        (~ log) (if valid "valid" "invalid"))
             (return-from locked-valid? valid)))

      (unlock-tvars acquired log))))



#-tx-hash
(defun locked-valid? (log)
  "Return T if LOG is valid and NIL if it's invalid.
Return :UNKNOWN if relevant locks could not be acquired."

  (declare (type tlog log))
  (let ((reads (tlog-reads log))
        (n-reads 0)
        (acquiring nil)
        (acquired (list nil)))
    (declare (type fixnum n-reads))

    (when (null reads)
      (return-from locked-valid? t))

    (unwind-protect
         (progn
           ;; we must lock TVARs that have been read: expensive,
           ;; but needed to ensure this threads sees other commits as atomic
           (loop for var in reads do
                (push var acquiring)
                (incf (the fixnum n-reads)))

           (unless (try-lock-tvars acquiring acquired n-reads log "rolled back")
             (return-from locked-valid? :unknown))

           (let1 valid (valid? log)
             (log:debug "Tlog ~A is ~A, verified with locks"
                        (~ log) (if valid "valid" "invalid"))
             (return-from locked-valid? valid)))

      (unlock-tvars acquired log))))


;;;; ** Rolling back


(defun cleanup-after-commit-or-rollback (log)
  (declare (type tlog log))

  #-tx-hash
  (unwind-protect
       (clean-tvars log)
    (release-tlog-id log))

  nil)


(declaim (inline rollback))
(defun rollback (log)
  (cleanup-after-commit-or-rollback log))

  
;;;; ** Committing


(defun ensure-tlog-before-commit (log)
  "Create tlog-before-commit log if nil, and return it."
  (declare (type tlog log))
  (the vector
    (or (tlog-before-commit log)
        (setf (tlog-before-commit log)
              (make-array '(1) :element-type 'function :fill-pointer 0 :adjustable t)))))

(defun ensure-tlog-after-commit (log)
  "Create tlog-after-commit log if nil, and return it."
  (declare (type tlog log))
  (the vector
    (or (tlog-after-commit log)
        (setf (tlog-after-commit log)
              (make-array '(1) :element-type 'function :fill-pointer 0 :adjustable t)))))




(defun call-before-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked immediately before the current transaction commits.

IMPORTANT: See BEFORE-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))
  (vector-push-extend func (ensure-tlog-before-commit log))
  func)

(defun call-after-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked after the current transaction commits.

IMPORTANT: See AFTER-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))
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
  (declare (type vector funcs))
  (loop for i from 0
     while (< i (length funcs))
     do
       (funcall (aref funcs i))))


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
          (log:trace "Tlog ~A before-commit wants to rerun" (~ log))
          (return-from invoke-before-commit nil)))))
  t)


(defun invoke-after-commit (log)
  "After committing, call in order all functions registered with (after-commit)
If any of them signals an error, it will be propagated to the caller
but the TLOG will remain committed."
  (declare (type tlog log))
  (when-bind funcs (tlog-after-commit log)
    ;; restore recording and log as the current tlog, functions may need them
    ;; to read transactional memory
    (with-recording-to-tlog log
      (loop-funcall-on-appendable-vector funcs)))
  t)


(defun commit (log)
  "Commit a TLOG to memory.

It returns a boolean specifying whether or not the transaction
log was committed.  If the transaction log cannot be committed
it either means that:
a) the TLOG is invalid - then the whole transaction must be re-executed
b) another TLOG is writing the same TVARs being committed
   so that TVARs locks could not be aquired - also in this case
   the whole transaction will be re-executed, as there is little hope
   that the TLOG will still be valid."
   
  (declare (type tlog log))
  (let ((reads (tlog-reads log))
        (writes (tlog-writes log))
        (n-vars 0)
        (subscript (the fixnum (tlog-id log)))
        (acquiring nil)
        (acquired (list nil))
        changed
        success)

    (declare (type fixnum n-vars)
             (ignorable subscript))

    ;; before-commit functions run without locks
    (unless (invoke-before-commit log)
      (return-from commit nil))

    (when #+tx-hash (zerop (hash-table-count writes))
          #-tx-hash (null writes)
      (log:debug "Tlog ~A committed (nothing to write)" (~ log))
      (invoke-after-commit log)
      (return-from commit t))

    #+never (log:trace "Tlog ~A committing..." (~ log))

    (unwind-protect
         (block nil
           ;; we must lock TVARs that have been read or written: expensive
           ;; but needed to ensure this threads sees other commits as atomic,
           ;; and other threads see this commit as atomic

           #+tx-hash
           (let ((big-hash   reads)
                 (small-hash writes))
             
             (when (< (hash-table-count big-hash) (hash-table-count small-hash))
               (rotatef big-hash small-hash))
 
             (setf acquiring (hash-table-keys big-hash))
             (setf n-vars (hash-table-count big-hash))
             (do-hash (var val) small-hash
               ;; do not put duplicates in ACQUIRING list
               (unless (nth-value 1 (get-hash big-hash var))
                 (push var acquiring)
                 (incf (the fixnum n-vars)))))

           #-tx-hash
           (progn
             (loop for var in reads do
                  (push var acquiring)
                  (incf (the fixnum n-vars)))
             (dolist (var writes)
               ;; do not put duplicates in ACQUIRING list
               (when (eq +unbound-tx+ (svref (tvar-tx-reads var) subscript))
                 (push var acquiring)
                 (incf (the fixnum n-vars)))))

           
           (unless (try-lock-tvars acquiring acquired n-vars log "committed")
             (return))
            
           ;; check for log validity one last time, with locks held.
           (when (invalid? log)
             (log:debug "Tlog ~A is invalid, not committed (checked with locks)" (~ log))
             (return))

           ;; COMMIT, i.e. actually write new values into TVARs

           #+tx-hash
           (do-hash (var val) writes
             (let1 current-val (raw-value-of var)
                   (unless (eq val current-val)
                     (setf (raw-value-of var) val)
                     (push var changed)
                     #+never (log:trace "Tlog ~A tvar ~A changed value from ~A to ~A"
                                        (~ log) (~ var) current-val val))))

           #-tx-hash
           (dolist (var writes)
             (let ((val (svref (tvar-tx-writes var) subscript))
                   (current-val (raw-value-of var)))
               (unless (eq val current-val)
                 (setf (raw-value-of var) val)
                 (push var changed)
                 (log:trace "Tlog ~A tvar ~A changed value from ~A to ~A"
                            (~ log) (~ var) current-val val))))

           (log:debug "Tlog ~A committed." (~ log))
           (setf success t))


      (unlock-tvars acquired log)

      (dolist (var changed)
        #+never (log:trace "Tlog ~A notifying threads waiting on tvar ~A"
                           (~ log) (~ var))
        (notify-tvar var))
      (when success
        ;; after-commit functions run without locks
        (invoke-after-commit log)))))
                   



;;;; ** Merging


(defun merge-tlog-reads (log1 log2)
  "Merge (tlog-reads LOG1) and (tlog-reads LOG2).

Return merged TLOG (either LOG1 or LOG2) if tlog-reads LOG1 and LOG2 are compatible,
i.e. if they contain the same values for the TVARs common to both, otherwise return NIL
\(in the latter case, the merge will not be completed)."
  (declare (type tlog log1 log2))

  #+tx-hash
  (let* ((reads1 (tlog-reads log1))
         (reads2 (tlog-reads log2))
         (n1 (hash-table-count reads1))
         (n2 (hash-table-count reads2)))
         
    (when (< n1 n2)
      (rotatef log1 log2)
      (rotatef reads1 reads2)
      (rotatef n1 n2)) ;; guarantees n1 >= n2

    (if (or (zerop n2) (merge-hash-tables reads1 reads2))
        log1
        nil))

  #-tx-hash
  (error "~A not implemented for fast-tx" 'merge-tlog-reads))

  


(defun commit-nested (log)
  "Commit LOG into its parent log; return LOG.

Unlike (commit log), this function is guaranteed to always succeed.

Implementation note: copy tlog-reads, tlog-writes, tlog-before-commit
and tlog-after-commit into parent, or swap them with parent"

  (declare (type tlog log))

  #+tx-hash
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

    log)

  #-tx-hash
  (error "~A not implemented for fast-tx" 'commit-nested))


