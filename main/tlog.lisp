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


(in-package :stmx)

(enable-#?-syntax)

;;;; * Transaction logs

;;;; ** thread-local TLOGs pool

(defun make-tlog-pool (&optional (n 10))
  (make-fast-vector n :element-type '(or null tlog) :initial-element nil))

(defparameter *tlog-pool* (make-tlog-pool))

(eval-always
  (ensure-thread-initial-binding '*tlog-pool* '(make-tlog-pool)))

(defun stmx-internal-error/suggest-bordeaux-threads (datum &rest arguments)
  (stmx-internal-error
   "~A~&~A"
   (apply #'format nil datum arguments)
"Typical cause is:
   new threads were created with implementation-specific functions,
   as for example (sb-thread:make-thread) or (mp:process-run-function),
   that do not apply thread-local bindings stored in
   bordeaux-threads:*default-special-bindings*
Solution:
   use (bordeaux-threads:make-thread) instead."))


(defun %validate-tlog-pool (&optional (pool *tlog-pool*))
  (declare (type fast-vector pool))

  (let ((i 0))
    (do-fast-vector (element) pool
      (unless element
        (stmx-internal-error/suggest-bordeaux-threads
         "inconsistent TLOG-POOL ~S: TLOG at index ~S is NIL." pool i))
      (incf (the fixnum i)))))

(defmacro validate-tlog-pool ()
  #+stmx-debug
  `(%validate-tlog-pool))


(declaim (inline validate-current-thread))

(defun validate-current-thread ()
  (let ((actual   *current-thread*)
        (expected (bt:current-thread)))
    (unless (eq actual expected)
      (stmx-internal-error/suggest-bordeaux-threads
       "stmx:*current-thread* contains a stale value:~&   found ~S~&   expecting ~S"
       actual expected))))

  
;;;; ** Creating, copying and clearing tlogs

                                            
(defun clear-tlog (log)
  "Remove all transactional reads and writes stored in LOG,
as well as functions registered with BEFORE-COMMIT and AFTER-COMMIT;
return LOG itself."
  (declare (type tlog log))

  (setf (tlog-parent log) nil)

  (clear-txhash (tlog-reads log))
  (clear-txhash (tlog-writes log))

  (clear-txfifo (tlog-locked log))

  (awhen (tlog-before-commit log) (setf (fill-pointer it) 0))
  (awhen (tlog-after-commit log)  (setf (fill-pointer it) 0))
  log)


(declaim (inline new-tlog))
(defun new-tlog ()
  "Get a TLOG from pool or create one, and return it."
  (let1 log (fast-vector-pop-macro *tlog-pool*
                                   (progn (validate-current-thread) (make-tlog)))
    (validate-tlog-pool)
    (setf (tlog-read-version (the tlog log)) +invalid-version+)
    log))


(declaim (inline free-tlog))
(defun free-tlog (log)
  "Return a no-longer-needed TLOG to the pool."
  (declare (type tlog log))
  ;; fix performance killer: if (tlog-reads log) or (tlog-writes log)
  ;; are very large, clearing them takes ages. In such case, better
  ;; to just discard the TLOG.
  (when (and
         (<= (txhash-table-count (tlog-reads log)) 127)
         (<= (txhash-table-count (tlog-writes log)) 127))
    (when (fast-vector-push log *tlog-pool*)
      (clear-tlog log))
    (validate-tlog-pool))
  nil)
    


(defun new-or-clear-tlog (log &key parent)
  "If LOG is not nil, clear it as per (clear-tlog), otherwise create
a new tlog as per (new-tlog). In both cases the tlog is returned,
and its parent is set to PARENT."
  (declare (type (or null tlog) log parent))
  (let1 log (if log
                (clear-tlog log)
                (new-tlog))

    (when parent
      (setf (tlog-parent log) parent)
      (copy-txhash-table-into (tlog-reads log)  (tlog-reads parent))
      (copy-txhash-table-into (tlog-writes log) (tlog-writes parent)))
    log))














;;;; ** Listening and waiting


(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))

  (let1 reads (tlog-reads log)

    (when (zerop (txhash-table-count reads))
      (stmx-internal-error "Transaction ~A called (retry), but no TVARs to wait for changes.
  This is a bug either in the STMX library or in the application code.
  Possible reason: some code analogous to (atomic (retry)) was executed.
  Such code is not allowed, because at least one TVAR or one TOBJ slot
  must be read before retrying an ATOMIC block." (~ log)))

    (do-txhash (var val) reads
      (unless (eq val (raw-value-of var))
        (log.debug "Tlog ~A: tvar ~A changed before listening, not sleeping"
                   (~ log) (~ var))
        (return-from listen-tvars-of nil))

      (listen-tvar var log)
      ;; to avoid deadlocks, check raw-value also AFTER listening on tvar.
      ;; otherwise if we naively
      ;;   1) first, check raw-value and decide whether to listen
      ;;   2) then, listen if we decided to
      ;; the tvar could change BETWEEN 1) and 2) and we would miss it
      ;; => DEADLOCK
      (unless (eq val (raw-value-of var))
        (unlisten-tvar var log)
        (log.debug "Tlog ~A: tvar ~A changed after  listening, not sleeping"
                   (~ log) (~ var))
        (return-from listen-tvars-of nil))

      (log.trace "Tlog ~A listening for tvar ~A changes"
                 (~ log) (~ var))))

  (return-from listen-tvars-of t))



(defun unlisten-tvars-of (log)
  "Un-listen on tvars, i.e. deregister not to get notifications if they change."

  (declare (type tlog log))

  (do-txhash (var) (tlog-reads log)
    (unlisten-tvar var log))
  (values))


      
(defun wait-once (log)
  "Sleep, i.e. wait for relevant TVARs to change.
Return T if slept, or NIL if some TVAR definitely changed before sleeping."

  (declare (type tlog log))
  (let ((lock (tlog-lock log))
        (prevent-sleep nil))

    (log.debug "Tlog ~A sleeping now" (~ log))

    (with-lock (lock)
      (unless (setf prevent-sleep (tlog-prevent-sleep log))
        (condition-wait (tlog-semaphore log) lock)))
      
    (when (log.debug)
      (if prevent-sleep
          (log.debug "Tlog ~A prevented from sleeping, some TVAR must have changed" (~ log))
          (log.debug "Tlog ~A woke up" (~ log))))
    (not prevent-sleep)))


(defun wait-tlog (log)
  "Wait until the TVARs read during transaction have changed. Return T."

  (declare (type tlog log))

  ;; lazily initialize (tlog-lock log) and (tlog-semaphore log)
  (when (null (tlog-lock log))
    (setf (tlog-lock log) (make-lock (format nil "~A-~A" 'tlog (~ log))))
    (setf (tlog-semaphore log) (make-condition-variable)))

  ;; we are going to sleep, unless some TVAR changes
  ;; and/or tells us not to.
  (with-lock ((tlog-lock log))
    (setf (tlog-prevent-sleep log) nil))

  ;; TVAR change notifications are not supported by HW transactions,
  ;; so we must disable them
  #?+hw-transactions (global-clock/incf-nohw-counter)

  (unwind-protect
       (when (listen-tvars-of log)
         (loop while (and (wait-once log) (valid? log))))

    ;; re-enable HW transactions
    #?+hw-transactions (global-clock/decf-nohw-counter)

    ;; call UNLISTEN-TVARS-OF also if an error is signaled
    (unlisten-tvars-of log))
  t)
      



(defun notify-tlog (log var)
  (declare (type tlog log)
           (type tvar var)
           (ignorable var))
  (log.debug "Waking up tlog ~A listening on tvar ~A" (~ log) (~ var))
  ;; Max, question: do we also need to acquire (tlog-lock log)?
  ;; Answering myself: YES! otherwise we can deadlock (tested, it happens)
  (with-lock ((tlog-lock log))
    (setf (tlog-prevent-sleep log) t)
    (condition-notify (tlog-semaphore log))))



;;;; ** Printing

(defprint-object (obj tlog)
  (format t "v~A" (~ obj)))
