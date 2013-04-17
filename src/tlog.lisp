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
  (make-array n :element-type 'tlog :fill-pointer 0))


(declaim (type (and (vector tlog) (not simple-array)) *tlog-pool*))
(defvar *tlog-pool* (make-tlog-pool))

(eval-when (:load-toplevel :execute)
  (ensure-thread-initial-bindings '(*tlog-pool* . (make-tlog-pool))))

;;;; ** Creating, copying and clearing tlogs

                                            
(defun clear-tlog (log)
  "Remove all transactional reads and writes stored in LOG,
as well as functions registered with BEFORE-COMMIT and AFTER-COMMIT;
return LOG itself."
  (declare (type tlog log))

  (setf (tlog-parent log) nil)

  #-tx-hash
  (setf (tlog-reads log) nil
        (tlog-writes log) nil)

  #+tx-hash
  (clrhash (tlog-reads log))
  #+tx-hash
  (clrhash (tlog-writes log))

  (awhen (tlog-before-commit log) (setf (fill-pointer it) 0))
  (awhen (tlog-after-commit log)  (setf (fill-pointer it) 0))
  log)

(defun new-tlog ()
  "Get a TLOG from pool or create one, and return it."
  (if (zerop (length *tlog-pool*))
      (make-tlog)
      (vector-pop *tlog-pool*)))

(defun free-tlog (log)
  "Return a no-longer-needed TLOG to the pool."
  (declare (type tlog log))

  (when (vector-push log *tlog-pool*)
    (clear-tlog log))
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
      #+tx-hash
      (copy-hash-table (tlog-reads log)  (tlog-reads parent))
      #+tx-hash
      (copy-hash-table (tlog-writes log) (tlog-writes parent))
      (setf (tlog-parent log) parent))
    log))



;;;; ** Acquiring and releasing TLOG id, cleaning up TVAR transactional cache

#-tx-hash
(eval-always
 (declaim (type list *tlog-id-list*))
 (defvar *tlog-id-list* '(0 1 2 3 4 5 6 7 8 9))
 (defvar *tlog-id-lock* (make-lock "TLOG-ID"))
 (defvar *tlog-id-semaphore* (make-condition-variable :name "TLOG-ID"))

 (defun acquire-tlog-id (log)
   (declare (type tlog log))
   (setf (tlog-id log) (pop *tlog-id-list*)))
 #|
         (with-lock-held (*tlog-id-lock*)
           (loop for id = (pop *tlog-id-list*)
              until id do
                (condition-wait *tlog-id-semaphore* *tlog-id-lock*)
              finally (return id)))))
 |#
         
 (defun release-tlog-id (log)
   (declare (type tlog log))

   (push (tlog-id log) *tlog-id-list*)
   #|
   (with-lock-held (*tlog-id-lock*)
     (let1 orig-list *tlog-id-list*
       (push (tlog-id log) *tlog-id-list*)
       (when (null orig-list)
         (condition-notify *tlog-id-semaphore*))))
   |#
   t)


 (defun clean-tvars (log)
   "Remove any cached transactional value in all TVARs read or written
while LOG was the current transaction log"
   (declare (type tlog log))

   (let1 subscript (the fixnum (tlog-id log))
     (dolist (var (tlog-reads log))
       (setf (svref (tvar-tx-reads var) subscript) +unbound-tx+))
     (dolist (var (tlog-writes log))
       (setf (svref (tvar-tx-writes var) subscript) +unbound-tx+)))))

  



;;;; ** Reads and writes

#+tx-hash
(defun tx-read-of (var &optional (log (current-tlog)))
  "If VAR's value is stored in transaction LOG, return the stored value.
Otherwise, add (raw-value-of VAR) as the read of VAR stored in transaction LOG
and return (raw-value-of VAR).

TX-READ-OF is an internal function called by ($ VAR) and by reading TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (multiple-value-bind (value present?)
      (get-hash (tlog-writes log) var)
    (when present?
      (return-from tx-read-of value)))

  (let1 reads (tlog-reads log)
    (multiple-value-bind (value present?)
        (get-hash reads var)
      (when present?
        (return-from tx-read-of value)))

    (set-hash reads var (raw-value-of var))))


#+tx-hash
(defun tx-write-of (var value &optional (log (current-tlog)))
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (set-hash (tlog-writes log) var value))


#-tx-hash
(defun tx-read-of (var &optional (log (current-tlog)))
  "If VAR's value is stored in transaction LOG, return the stored value.
Otherwise, add (raw-value-of VAR) as the read of VAR stored in transaction LOG
and return (raw-value-of VAR).

TX-READ-OF is an internal function called by ($ VAR) and by reading TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (let1 subscript (tlog-id log)

    (let1 value (svref (tvar-tx-writes var) subscript)
      (unless (eq +unbound-tx+ value)
        (return-from tx-read-of value)))

    (let1 reads-array (tvar-tx-reads var)

      (let1 value (svref reads-array subscript)
         (unless (eq +unbound-tx+ value)
           (return-from tx-read-of value)))

      (push var (tlog-reads log))
      (setf (svref reads-array subscript) (raw-value-of var)))))


#-tx-hash
(defun tx-write-of (var value &optional (log (current-tlog)))
  "Store in transaction LOG the writing of VALUE into VAR; return VALUE.

TX-WRITE-OF is an internal function called by (setf ($ VAR) VALUE)
and by writing TOBJs slots."
  (declare (type tvar var)
           (type tlog log))

  (let ((writes-array (tvar-tx-writes var))
        (subscript (tlog-id log)))

    (let1 value (svref writes-array subscript)
      (when (eq +unbound-tx+ value)
        (push var (tlog-writes log))))

    (setf (svref writes-array subscript) value)))






;;;; ** Listening and waiting


#+tx-hash
(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (tlog-reads log)

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


#-tx-hash
(defun listen-tvars-of (log)
  "Listen on tvars recorded in LOG, i.e. register to get notified if they change.
Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (tlog-reads log)

    (when (null reads)
      (error "BUG! Transaction ~A called (retry), but no TVARs to wait for changes.
  This is a bug either in the STMX library or in the application code.
  Possible reason: some code analogous to (atomic (retry)) was executed.
  Such code is not allowed, because at least one TVAR or one TOBJ slot
  must be read before retrying an ATOMIC block." (~ log)))

    (dolist (var reads)
      (listen-tvar var log)
      ;; to avoid deadlocks, check raw-value AFTER listening on tvar.
      ;; otherwise if we naively
      ;;   1) first, check raw-value and decide whether to listen
      ;;   2) then, listen if we decided to
      ;; the tvar could change BETWEEN 1) and 2) and we would miss it
      ;; => DEADLOCK
      (let1 val (svref (tvar-tx-reads var) (tlog-id log))
        (if (eq val (raw-value-of var))
            (log:trace "Tlog ~A listening for tvar ~A changes"
                       (~ log) (~ var))
            (progn
              (unlisten-tvar var log)
              (log:debug "Tlog ~A: tvar ~A changed, not going to sleep"
                         (~ log) (~ var))
              (return-from listen-tvars-of nil)))))
    (return-from listen-tvars-of t)))



(defun unlisten-tvars-of (log)
  "Un-listen on tvars, i.e. deregister not to get notifications if they change."

  (declare (type tlog log))
  #+tx-hash
  (do-hash (var val) (tlog-reads log)
    (unlisten-tvar var log))
  #-tx-hash
  (dolist (var (tlog-reads log))
     (unlisten-tvar var log))
  (values))



      
(defun wait-once (log)
  "sleep, i.e. wait for relevat tvars to change.
after sleeping, return t if log is valid, otherwise return nil."

  (declare (type tlog log))
  (log:debug "tlog ~a sleeping now" (~ log))
  (let ((lock (tlog-lock log))
        (valid t))

    (with-lock-held (lock)
      (when (setf valid (not (tlog-prevent-sleep log)))
        (condition-wait (tlog-semaphore log) lock)))

    (if valid
        (progn
          (log:debug "tlog ~a woke up" (~ log))
          (setf valid (valid? log)))
        (log:debug "tlog ~a prevented from sleeping, some tvar must have changed" (~ log)))
    valid))


(defun wait-tlog (log &key once)
  "wait until the tvars read during transaction have changed.
return t if log is valid after sleeping, otherwise return nil.

note from (cmt paragraph 6.3):
when some other threads notifies the one waiting in this function,
check if the log is valid. in case it's valid, re-executing
the last transaction is useless: it means the relevant tvars
did not change, but the transaction is waiting for them to change"

  (declare (type tlog log)
           (type boolean once))

  ;; lazily initialize (tlog-lock log) and (tlog-semaphore log)
  (when (null (tlog-lock log))
    (setf (tlog-lock log) (make-lock (format nil "~a-~a" 'tlog (~ log))))
    (setf (tlog-semaphore log) (make-condition-variable)))

  ;; we are going to sleep, unless some tvar changes
  ;; and/or tells us not to.
  (with-lock-held ((tlog-lock log))
    (setf (tlog-prevent-sleep log) nil))

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
  ;; Max, question: do we also need to acquire (tlog-lock log)?
  ;; Answering myself: YES! otherwise we can deadlock (tested, it happens)
  (with-lock-held ((tlog-lock log))
    (setf (tlog-prevent-sleep log) t)
    (condition-notify (tlog-semaphore log))))



;;;; ** Printing

(defprint-object (obj tlog)
  (format t "~A" (tlog-id obj)))
