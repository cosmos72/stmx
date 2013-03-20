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
           :reads (copy-hash-table (reads-of parent))
           :writes (copy-hash-table (writes-of parent)))
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
  (dohash (reads-of log) var val
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


(defun lock-tvar-nonblock (var log)
  "Try to acquire VAR lock non-blocking. Return t if acquired, else return nil."

  (declare (type tvar var)
	   (type tlog log))
  (if (acquire-lock (lock-of var) nil)
      (progn
	(log:trace "Tlog ~A locked tvar ~A" (~ log) (~ var))
	t)
      (progn
	(log:debug "Tlog ~A ...not committed: could not lock tvar ~A"
		   (~ log) (~ var))
	nil)))



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
	(acquired nil)
	(changed nil))

    (when (zerop (hash-table-count writes))
      (log:debug "Tlog ~A ...committed (nothing to write)" (~ log))
      (return-from commit t))

    (log:trace "Tlog ~A committing..." (~ log))
    (unwind-protect
         (progn
           (dohash reads var val
             (if (lock-tvar-nonblock var log)
	       (push var acquired)
	       (return-from commit nil)))
           (unless (valid? log)
             (log:debug "Tlog ~A ...not committed: log is invalid" (~ log))
             (return-from commit nil))

	   ;; we must lock also TVARs that will be written: expensive,
	   ;; but needed to ensure other threads see the writes as atomic
           (dohash writes var val
	     (multiple-value-bind (dummy read?) (gethash var reads)
	       (declare (ignore dummy))
	       ;; skip locking TVARs present in (reads-of log), we already locked them
	       (when (not read?)
		 (if (lock-tvar-nonblock var log)
		     (push var acquired)
		     (return-from commit nil)))))

	   ;; actually write new values into TVARs
           (dohash writes var val
	     (let1 actual-val (raw-value-of var)
	       (when (not (eq val actual-val))
		 (setf (raw-value-of var) val)
		 (push var changed)
		 (log:trace "Tlog ~A tvar ~A value changed from ~A to ~A"
			    (~ log) (~ var) actual-val val))))

           (log:debug "Tlog ~A ...committed" (~ log))
           (return-from commit t))

      (dolist (var acquired)
        (release-lock (lock-of var))
        (log:trace "Tlog ~A unlocked tvar ~A" (~ log) (~ var)))
      (dolist (var changed)
	(log:trace "Tlog ~A notifying threads waiting on tvar ~A"
		   (~ log) (~ var))
        (notify-tvar var)))))


;;;; ** Merging


(defun merge-tlogs (log1 log2)
  "Merge LOG2 into LOG1; return LOG1."

  (declare (type tlog log1 log2))

  (let ((h1 (writes-of log1))
        (h2 (writes-of log2)))
    (dohash h2 var val
      (set-hash h1 var val)))

  (let ((h1 (reads-of log1))
        (h2 (reads-of log2)))
    (dohash h2 var val
      (set-hash h1 var val)))

  log1)

(defun compatible-tlogs (log1 log2)
  "Return t if LOG1 and LOG2 are compatible, i.e. if they contain
the same values for the TVARs present in both their (reads-of)."

  (declare (type tlog log1 log2))
  (let ((reads1 (reads-of log1))
        (reads2 (reads-of log2)))
        
    (when (> (hash-table-count reads1) (hash-table-count reads2))
      (rotatef reads1 reads2))

    (dohash reads1 var val
      (unless (eq val (gethash var reads2))
        (return-from compatible-tlogs nil)))
    t))



;;;; ** Listening and waiting


(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (reads-of log)

    (when (zerop (hash-table-count reads))
      (error "BUG! Tried to wait on TLOG ~A, but no TVARs to wait on.~%  This is a bug either in the STMX library or in the application code.~%  Possible reason: some code analogous to (atomic (retry)) was executed.~%  Such code is not allowed, because at least one TVAR or one TOBJ slot~%  must be read before retrying an ATOMIC block." (~ log)))

    (dohash reads var val
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

    (dohash reads var val
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
  ;; Question: do we also need to acquire (lock-of log)?
  ;; Answer: YES! otherwise we can deadlock (tested, it happens)
  (with-lock-held ((lock-of log))
    (setf (prevent-sleep-of log) t)
    (condition-notify (semaphore-of log))))
