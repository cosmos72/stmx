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

;;;; ** thread-local TLOGs pool

(declaim (type fixnum *tlog-pool-max-idle*))
(defvar *tlog-pool-max-idle* 10)

(defun make-tlog-pool (&optional (n *tlog-pool-max-idle*))
  (declare (type fixnum n))
  (make-array (list n) :element-type 'tlog :fill-pointer 0))


(declaim (type (and (vector tlog) (not simple-array)) *tlog-pool*))
(defvar *tlog-pool* (make-tlog-pool))

(eval-always
  (unless (assoc '*tlog-pool* bt:*default-special-bindings*)
    (push '(*tlog-pool* . (make-tlog-pool)) bt:*default-special-bindings*)))


;;;; ** Creating, copying and clearing tlogs

                                            
(defun clear-tlog (log)
  "Remove all transactional reads and writes stored in LOG,
as well as functions registered with BEFORE-COMMIT and AFTER-COMMIT;
return LOG itself."
  (declare (type tlog log))

  (setf (parent-of log) nil)

  (clrhash (reads-of log))
  (clrhash (writes-of log))

  (awhen (before-commit-of log) (setf (fill-pointer it) 0))
  (awhen (after-commit-of log)  (setf (fill-pointer it) 0))
  log)


(defun make-tlog ()
  "Get a TLOG from pool or create one, and return it."
  (if (zerop (length *tlog-pool*))
      (new 'tlog)
      (vector-pop *tlog-pool*)))


(defun free-tlog (log)
  "Return a no-longer-needed TLOG to the pool."
  (declare (type tlog log))
  (when (vector-push log *tlog-pool*)
    (clear-tlog log))
  nil)
    


(defun make-or-clear-tlog (log &key parent)
  "If LOG is not nil, clear it as per (clear-tlog), otherwise create
a new tlog as per (make-tlog). In both cases the tlog is returned,
and its parent is set to PARENT."
  (declare (type (or null tlog) log parent))
  (let1 log (if log
                (clear-tlog log)
                (make-tlog))

    (when parent
      (setf (parent-of log) parent)
      (copy-hash-table (reads-of log)  (reads-of parent))
      (copy-hash-table (writes-of log) (writes-of parent)))
    log))



;;;; ** Reads and writes


(defun tx-read-of (var &optional (log (current-tlog)))
  "If VAR's value is stored in transaction LOG, return the stored value.
Otherwise, add (raw-value-of VAR) as the read of VAR stored in transaction LOG
and return (raw-value-of VAR).

TX-READ-OF is an internal function called by ($ VAR) and by reading TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (multiple-value-bind (value present?)
      (gethash var (writes-of log))
    (when present?
      (return-from tx-read-of value)))

  (let1 reads (reads-of log)
    (multiple-value-bind (value present?)
        (gethash var reads)
      (when present?
        (return-from tx-read-of value)))

    (setf (gethash var reads) (raw-value-of var))))


(defun tx-write-of (var value &optional (log (current-tlog)))
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (setf (gethash var (writes-of log)) value))








;;;; ** Listening and waiting


(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (reads-of log)

    (when (zerop (hash-table-count reads))
      (error "BUG! Transaction ~A called (retry), but no TVARs to wait for changes.
  This is a bug either in the STMX library or in the application code.
  Possible reason: some code analogous to (atomic (retry)) was executed.
  Such code is not allowed, because at least one TVAR or one TOBJ slot
  must be read before retrying an ATOMIC block." (~ log)))

    (do-hash (var val) reads
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
  (do-hash (var val) (reads-of log)
    (unlisten-tvar var log))
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
