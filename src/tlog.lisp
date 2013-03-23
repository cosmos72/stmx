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

;;;; * Transaction logs

;;;; ** Creating and clearing tlogs

(defun make-tlog (&key parent)
  "Create and return a tlog.
If PARENT is not nil, it will be the parent of returned tlog"
  (declare (type (or null tlog) parent))
  (if parent
      (new 'tlog :parent parent
           :reads (clone-hash-table (reads-of parent))
           :writes (clone-hash-table (writes-of parent)))
      (new 'tlog)))

                                            
(defun clear-tlog (log &key parent)
  "Remove all transactional reads and writes stored in LOG; return LOG itself.
If PARENT is not nil, it will be set as the parent of LOG."
  (declare (type tlog log)
           (type (or null tlog) parent))
  (reset-hash-table (reads-of log)  :defaults (when parent (reads-of parent)))
  (reset-hash-table (writes-of log) :defaults (when parent (writes-of parent)))
  log)


(defun make-or-clear-tlog (log &key parent)
  "If LOG is not nil, clear it as per (clear-tlog), otherwise create
a new tlog as per (make-tlog). In both cases the tlog is returned,
and its parent is set to PARENT."
  (declare (type (or null tlog) log parent))
  (if log
      (clear-tlog log :parent parent)
      (make-tlog :parent parent)))



;;;; ** Validating and committing

(defun valid? (log)
  "Return t if a TLOG is valid, i.e. it contains an up-to-date view
of TVARs that were read during the transaction."

  (declare (type tlog log))
  (log:trace "Tlog ~A valid?.." (~ log))
  (dohash (var val) (reads-of log)
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

(declaim (inline invalid?))
(defun invalid? (log)
  "Return (not (valid? LOG))."
  (declare (type tlog log))
  (not (valid? log)))



(defun try-lock-tvar (var log txt)
  "Try to acquire VAR lock non-blocking. Return t if acquired, else return nil."
  (declare (type tvar var)
	   (type tlog log)
           (type string txt))
  (if (acquire-lock (lock-of var) nil)
      (progn
	(log:trace "Tlog ~A locked tvar ~A" (~ log) (~ var))
	t)
      (progn
	(log:debug "Tlog ~A not ~A: could not lock tvar ~A"
		   (~ log) txt (~ var))
	nil)))


(defun unlock-tvars (vars log)
  (declare (type list vars)
	   (type tlog log))
  (dolist (var vars)
    (release-lock (lock-of var))
    (log:trace "Tlog ~A unlocked tvar ~A" (~ log) (~ var))))



(defun locked-valid? (log)
  "Return T if LOG is valid and NIL if it's invalid.
Return :UNKNOWN if relevant locks could not be acquired."

  (declare (type tlog log))
  (let ((reads (reads-of log))
	acquired)

    (unwind-protect
         (progn
	   ;; we must lock TVARs that have been read: expensive,
	   ;; but needed to ensure this threads sees other commits as atomic
           (dohash (var val) reads
             (if (try-lock-tvar var log "rolled back")
	       (push var acquired)
	       (return-from locked-valid? :unknown)))
           (let1 valid (valid? log)
             (log:debug "Tlog ~A is ~A, verified with locks"
                        (~ log) (if valid "valid" "invalid"))
             (return-from locked-valid? valid)))

      (unlock-tvars acquired log))))



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
	acquired
        changed)

    (when (zerop (hash-table-count writes))
      (log:debug "Tlog ~A committed (nothing to write)" (~ log))
      (return-from commit t))

    (log:trace "Tlog ~A committing..." (~ log))
    (unwind-protect
         (progn
	   ;; we must lock TVARs that have been read: expensive,
	   ;; but needed to ensure this threads sees other commits as atomic
           (dohash (var val) reads
             (if (try-lock-tvar var log "committed")
	       (push var acquired)
	       (return-from commit nil)))
           (when (invalid? log)
             (log:debug "Tlog ~A not committed: log is invalid" (~ log))
             (return-from commit nil))

	   ;; we must also lock TVARs that will be written: expensive,
	   ;; but needed to ensure other threads see this commit as atomic
           (dohash (var val) writes
	       ;; skip locking TVARs present in (reads-of log), we already locked them
	     (multiple-value-bind (dummy read?) (gethash var reads)
	       (declare (ignore dummy))
	       (when (not read?)
		 (if (try-lock-tvar var log "committed")
		     (push var acquired)
		     (return-from commit nil)))))

	   ;; actually write new values into TVARs
           (dohash (var val) writes
             (let1 actual-val (raw-value-of var)
               (when (not (eq val actual-val))
                 (setf (raw-value-of var) val)
                 (push var changed)
                 (log:trace "Tlog ~A tvar ~A value changed from ~A to ~A"
                            (~ log) (~ var) actual-val var))))

           (log:debug "Tlog ~A committed" (~ log))
           (return-from commit t))

      (unlock-tvars acquired log)

      (dolist (var changed)
        (log:trace "Tlog ~A notifying threads waiting on tvar ~A"
                   (~ log) (~ var))
        (notify-tvar var)))))


;;;; ** Merging


(declaim (inline merge-reads-of))
(defun merge-reads-of (log1 log2)
  "Copy reads of LOG2 into LOG1; return LOG1.

Used to merge reads-of two different nested transactions
in order to wait on their union."

  (declare (type tlog log1 log2))
  (copy-hash-table (reads-of  log1) (reads-of  log2))
  log1)


(defun commit-nested (log)
  "Commit LOG into its parent log; return LOG.

Unlike (commit log), this function is guaranteed to always succeed."

  (declare (type tlog log))
  (let1 parent (the tlog (parent-of log))
    (setf (reads-of parent) (reads-of log))
    (setf (writes-of parent) (writes-of log)))
  log)

(defun compatible-tlogs (log1 log2)
  "Return t if LOG1 and LOG2 are compatible, i.e. if they contain
the same values for the TVARs present in both their (reads-of)."

  (declare (type tlog log1 log2))
  (let ((reads1 (reads-of log1))
        (reads2 (reads-of log2)))
        
    ;; choose the smaller hash table for looping
    (when (> (hash-table-count reads1) (hash-table-count reads2))
      (rotatef reads1 reads2))

    (dohash (var val1) reads1
      (multiple-value-bind (val2 present2?) (gethash var reads2)
        (when (and present2? (not (eq val1 val2)))
          (return-from compatible-tlogs nil))))
    t))



;;;; ** Listening and waiting


(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (reads-of log)

    (when (zerop (hash-table-count reads))
      (error "BUG! Tried to wait on TLOG ~A, but no TVARs to wait on.~%  This is a bug either in the STMX library or in the application code.~%  Possible reason: some code analogous to (atomic (retry)) was executed.~%  Such code is not allowed, because at least one TVAR or one TOBJ slot~%  must be read before retrying an ATOMIC block." (~ log)))

    (dohash (var val) reads
      (listen-tvar var log)
      ;; to avoid deadlocks, check raw-value AFTER listening on tvar.
      ;; otherwise if we naively
      ;;   1) first, check raw-value and decide whether to listen
      ;;   2) then, listen if we decided to
      ;; the tvar could change BETWEEN 1) and 2) and we would miss it
      ;; => DEADLOCK
      (if (eq val (raw-value-of var))
	  (log:trace "Tlog ~A listening for tvar ~A changes"
		     (~ log) (~ var))
	  (progn
	    (unlisten-tvar var log)
	    (log:debug "Tlog ~A: tvar ~A changed, not going to sleep"
		       (~ log) (~ var))
	    (return-from listen-tvars-of nil)))))
  (return-from listen-tvars-of t))



(defun unlisten-tvars-of (log)
  "Un-listen on tvars, i.e. deregister not to get notifications if they change."

  (declare (type tlog log))
  (let1 reads (reads-of log)

    (dohash (var val) reads
      (unlisten-tvar var log)))
  (values))



      
(defun wait-once (log)
  "Sleep, i.e. wait for relevat TVARs to change.
After sleeping, return t if log is valid, otherwise return nil."

  (declare (type tlog log))
  (log:debug "Tlog ~A sleeping now" (~ log))
  (let ((lock (lock-of log))
	(valid t))

    (with-lock-held (lock)
      (when (setf valid (not (prevent-sleep-of log)))
	(condition-wait (semaphore-of log) lock)))

    (if valid
	(progn
	  (log:debug "Tlog ~A woke up" (~ log))
	  (setf valid (valid? log)))
	(log:debug "Tlog ~A prevented from sleeping, some TVAR must have changed" (~ log)))
    valid))


(defun wait-tlog (log &key once)
  "Wait until the TVARs read during transaction have changed.
Return t if log is valid after sleeping, otherwise return nil.

Note from (CMT paragraph 6.3):
When some other threads notifies the one waiting in this function,
check if the log is valid. In case it's valid, re-executing
the last transaction is useless: it means the relevant TVARs
did not change, but the transaction is waiting for them to change"

  (declare (type tlog log)
           (type boolean once))

  ;; lazily initialize (lock-of log) and (semaphore-of log)
  (when (null (lock-of log))
    (setf (lock-of log) (make-lock (format nil "~A-~A" 'tlog (~ log))))
    (setf (semaphore-of log) (make-condition-variable)))

  ;; we are going to sleep, unless some TVAR changes
  ;; and/or tells us not to.
  (with-lock-held ((lock-of log))
    (setf (prevent-sleep-of log) nil))

  (prog1
      (and (listen-tvars-of log)
	   (if once
	       (wait-once log)
	       (loop while (wait-once log))))
    (unlisten-tvars-of log)))
      



(defun notify-tlog (log var)
  (declare (type tlog log)
	   (type tvar var))
  (log:debug "Waking up tlog ~A listening on tvar ~A" (~ log) (~ var))
  ;; Max, question: do we also need to acquire (lock-of log)?
  ;; Answering myself: YES! otherwise we can deadlock (tested, it happens)
  (with-lock-held ((lock-of log))
    (setf (prevent-sleep-of log) t)
    (condition-notify (semaphore-of log))))
