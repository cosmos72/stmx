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


(defun valid? (log)
  "Return t if a TLOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction."

  (declare (type tlog log))
  (log:trace "Tlog ~A valid?.." (~ log))
  (awhen (reads-of log)
    (do-hash (var val) it
      (let1 actual-val (raw-value-of var)
        (if (eq val actual-val)
            (log:trace "Tlog ~A tvar ~A is up-to-date" (~ log) (~ var))
            (progn
              (log:trace "Tlog ~A conflict for tvar ~A: expecting ~A, found ~A"
                         (~ log) (~ var) (~ val) (~ actual-val))
              (log:debug "Tlog ~A ..not valid" (~ log))
              (return-from valid? nil))))))
  (log:trace "Tlog ~A ..is valid" (~ log))
  (return-from valid? t))


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


(defun sxhash< (arg1 arg2)
  "Compare the hash codes obtained with SXHASH for arg1 and arg2.
Return (< (sxhash arg1) (sxhash arg2))."
  (< (sxhash arg1) (sxhash arg2)))


(defun try-lock-tvar (var log txt)
  "Try to acquire VAR lock non-blocking. Return t if acquired, else return nil."
  (declare (type tvar var)
           (type tlog log)
           (type string txt))
  (let1 flag (acquire-lock (lock-of var) nil)
    (if flag
        (log:trace "Tlog ~A locked tvar ~A" (~ log) (~ var))
        (log:debug "Tlog ~A not ~A: could not lock tvar ~A"
                   (~ log) txt (~ var)))
    flag))


(defun try-lock-tvars (vars locked-vars log txt)
  "Sort VARS in \"address\" order - actually in (sxhash ...) order -
then non-blocking acquire their locks in such order.
Reason: acquiring in unspecified order may cause livelock, as two transactions
may repeatedly try acquiring the same two TVARs in opposite order.

Destructively modifies VARS.

Return t if all VARS were locked successfully, otherwise return nil.
In both cases, after return LOCKED-VARS will contain the locked tvars, listed
in reverse order: from last acquired to first acquired."
  (declare (type list vars)
           (type cons locked-vars)
           (type tlog log))

  ;;(log:user5 "unsorted TVARs to lock: (~{~A~^ ~})" vars)
  (setf vars (sort vars #'sxhash<))
  ;;(log:user5 "  sorted TVARs to lock: (~{~A~^ ~})" vars)

  (let1 acquired nil
    (unwind-protect
         (loop for cell = vars then tail
            while cell
            for tail = (rest cell)
            for var = (first cell)
            always (try-lock-tvar var log txt)
            do (setf (rest cell) acquired)
              (setf acquired cell))
              ;; (log:user5 "locked TVARs: (~{~A~^ ~})" acquired)
      (setf (first locked-vars) (first acquired))
      (setf (rest locked-vars) (rest acquired)))))


(defun unlock-tvars (vars log)
  "Release locked VARS in reverse order of acquisition."
  (declare (type list vars)
           (type tlog log))
  (when (first vars) ;; vars may be (NIL), ugly but needed by (try-lock-tvars ...) above
    (loop for var in vars do
         (release-lock (lock-of var))
         (log:trace "Tlog ~A unlocked tvar ~A" (~ log) (~ var)))))



(defun locked-valid? (log)
  "Return T if LOG is valid and NIL if it's invalid.
Return :UNKNOWN if relevant locks could not be acquired."

  (declare (type tlog log))
  (let ((reads (reads-of log))
        (acquiring nil)
        (acquired (list nil)))

    (when (empty-hash-table-or-nil reads)
      (return-from locked-valid? t))

    (unwind-protect
         (progn
           ;; we must lock TVARs that have been read: expensive,
           ;; but needed to ensure this threads sees other commits as atomic
           (setf acquiring (hash-table-keys reads))

           (unless (try-lock-tvars acquiring acquired log "rolled back")
             (return-from locked-valid? :unknown))

           (let1 valid (valid? log)
             (log:debug "Tlog ~A is ~A, verified with locks"
                        (~ log) (if valid "valid" "invalid"))
             (return-from locked-valid? valid)))

      (unlock-tvars acquired log))))



;;;; ** Committing


(defun ensure-before-commit-of (log)
  "Create before-commit-of log if nil, and return it."
  (declare (type tlog log))
  (the vector
    (or (before-commit-of log)
        (setf (before-commit-of log)
              (make-array '(1) :element-type 'function :fill-pointer 0 :adjustable t)))))

(defun ensure-after-commit-of (log)
  "Create after-commit-of log if nil, and return it."
  (declare (type tlog log))
  (the vector
    (or (after-commit-of log)
        (setf (after-commit-of log)
              (make-array '(1) :element-type 'function :fill-pointer 0 :adjustable t)))))




(defun call-before-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked immediately before the current transaction commits.

IMPORTANT: See BEFORE-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))
  (vector-push-extend func (ensure-before-commit-of log))
  func)

(defun call-after-commit (func &optional (log (current-tlog)))
  "Register FUNC function to be invoked after the current transaction commits.

IMPORTANT: See AFTER-COMMIT for what FUNC must not do."
  (declare (type function func)
           (type tlog log))
  (vector-push-extend func (ensure-after-commit-of log))
  func)



(defmacro before-commit (&body body)
  "Register BODY to be invoked immediately before the current transaction commits.
If BODY signals an error when executed, the error is propagated
to the caller and the transaction rollbacks.
Also, further code registered with before-commit is not executed.

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
to the caller and further code registered with after-commit is not executed,
but the transaction remains committed.

WARNING: Code registered with after-commit has a number or restrictions:

1) BODY must not write to *any* transactional memory: the consequences
are undefined.

2) BODY can only read from transactional memory already read or written
during the transaction. Reading from other transactional memory
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


(defun invoke-before-commit-of (log)
  "Before committing, call in order all functions registered
with (before-commit)
If any of them signals an error, the transaction will rollback
and the error will be propagated to the caller"
  (declare (type tlog log))
  (when-bind funcs (before-commit-of log)
    ;; restore recording and log as the current tlog, functions may need them
    ;; to read and write transactional memory
    (with-recording-to-tlog log
      (handler-case
          (loop-funcall-on-appendable-vector funcs)
        (rerun-error ()
         (log:trace "Tlog ~A before-commit wants to rerun" (~ log))
         (return-from invoke-before-commit-of nil)))))
  t)


(defun invoke-after-commit-of (log)
  "After committing, call in order all functions registered with (after-commit)
If any of them signals an error, it will be propagated to the caller
but the TLOG will remain committed."
  (declare (type tlog log))
  (when-bind funcs (after-commit-of log)
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
  (let ((reads (reads-of log))
        (writes (writes-of log))
        (acquiring nil)
        (acquired (list nil))
        changed
        success)

    ;; before-commit functions run without locks
    (unless (invoke-before-commit-of log)
      (return-from commit nil))

    (when (empty-hash-table-or-nil writes)
      (log:debug "Tlog ~A committed (nothing to write)" (~ log))
      (invoke-after-commit-of log)
      (return-from commit t))

    (log:trace "Tlog ~A committing..." (~ log))

    (unwind-protect
         (progn
           ;; we must lock TVARs that have been read or written: expensive,
           ;; but needed to ensure this threads sees other commits as atomic
           (setf acquiring (hash-table-keys (if reads reads writes)))
           (when reads
               (do-hash (var val) writes
                 (multiple-value-bind (read-val read?) (gethash var reads)
                   (declare (ignore read-val))
                   ;; do not put duplicates in ACQUIRING list
                   (when (not read?)
                     (push var acquiring)))))

           (unless (try-lock-tvars acquiring acquired log "committed")
             (return-from commit nil))

           ;; check for log validity one last time, with locks held.
           (when (invalid? log)
             (log:debug "Tlog ~A is invalid, not committed (checked with locks)" (~ log))
             (return-from commit nil))

           ;; COMMIT, i.e. actually write new values into TVARs
           (do-hash (var val) writes
             (let1 current-val (raw-value-of var)
               (when (not (eq val current-val))
                 (setf (raw-value-of var) val)
                 (push var changed)
                 (log:trace "Tlog ~A tvar ~A changed value from ~A to ~A"
                            (~ log) (~ var) current-val val))))

           (log:debug "Tlog ~A committed." (~ log))

           (return-from commit (setf success t)))

      (unlock-tvars acquired log)

      (dolist (var changed)
        (log:trace "Tlog ~A notifying threads waiting on tvar ~A"
                   (~ log) (~ var))
        (notify-tvar var))

      (when success
        ;; after-commit functions run without locks
        (invoke-after-commit-of log)))))
                   



;;;; ** Merging


(defun merge-reads-of (log1 log2)
  "Copy (reads-of LOG2) into (reads-of LOG1).

Return T if reads-of LOG1 and LOG2 are compatible, i.e. if they contain
the same values for the TVARs common to both, otherwise return NIL
\(in the latter case, the merge will not be completed)."
  (declare (type tlog log1 log2))
  (let ((reads1 (reads-of log1))
        (reads2 (reads-of log2)))
    
    (if reads1
        (if reads2
            (merge-hash-tables reads1 reads2)
            t)
        (progn
          (setf (reads-of log1) reads2)
          t))))
  


(defun commit-nested (log)
  "Commit LOG into its parent log; return LOG.

Unlike (commit log), this function is guaranteed to always succeed.

Implementation note: copy reads-of, writes-of, before-commit-of
and after-commit-of"

  (declare (type tlog log))
  (let1 parent (the tlog (parent-of log))
    (setf (reads-of parent) (reads-of log))
    (setf (writes-of parent) (writes-of log))

    (when-bind funcs (before-commit-of log)
      (let1 parent-funcs (ensure-before-commit-of parent)
        (loop for func across funcs do
             (vector-push-extend func parent-funcs))))

    (when-bind funcs (after-commit-of log)
      (let1 parent-funcs (ensure-after-commit-of parent)
        (loop for func across funcs do
             (vector-push-extend func parent-funcs)))))
  log)

