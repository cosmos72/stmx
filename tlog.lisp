;; -*- lisp -*-

(in-package :stmx)

;;;; * Transaction logs

;;;; ** Committing

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
   the whole transaction must be re-executed, as there is little hope
   that the TLOG will still be valid."
   
  (declare (type tlog log))
  (let ((reads (reads-of log))
	(writes (writes-of log))
	(acquired nil)
	(changed nil))
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
	   ;; but needed to ensure other threads sees the writes as atomic
           (dohash writes var val
	     (multiple-value-bind (dummy read?) (gethash var reads)
	       ;; skip locking TVARs present in (reads-of log), we already locked them
	       (when (not read?)
		 (if (lock-tvar-nonblock var log)
		     (push var acquired)
		     (return-from commit nil)))))

	   ;; actually write new values into TVARs
           (dohash writes var val
	     (let1 old-val (raw-value-of var)
	       (when (not (eq val old-val))
		 (setf (raw-value-of var) val)
		 (push var changed)
		 (log:trace "Tlog ~A tvar ~A value changed from ~A to ~A"
			    (~ log) (~ var) old-val val))))

           (log:debug "Tlog ~A ...committed" (~ log))
           (return-from commit t))

      (dolist (var acquired)
        (release-lock (lock-of var))
        (log:trace "Tlog ~A unlocked tvar ~A" (~ log) (~ var)))
      (dolist (var changed)
        (notify-tvar var)
        (log:trace "Tlog ~A notified threads waiting on tvar ~A" (~ log) (~ var))))))


;;;; ** Merging

(defun merge-tlogs (log1 log2)
  ;; TODO Max: merge-tlogs must be revised when implementing orelse
  (declare (type tlog log1 log2))
  (dohash (writes-of log2) var val
    (setf (gethash var (writes-of log1)) val))
  (dohash (reads-of log2) var ver
    (setf (gethash var (reads-of log1))
          (max ver
               (aif2 (gethash var (reads-of log1))
                     it
                     0))))
  log1)

;;;; ** Waiting


(defun listen-tvars-of (log)
  "Listen on tvars, i.e. register to get notified if they change.

Return t if log is valid and wait-tlog should sleep, otherwise return nil."

  (declare (type tlog log))
  (let1 reads (reads-of log)

    (when (zerop (hash-table-count reads))
      (error "Tried to wait on TLOG ~A, but no TVARs to wait on.~%  This is a BUG either in the STMX library or in the application code!~%  Possible reason is some application code analogous to (atomic (retry))~%  Such code is not allowed and will signal the current error~%  because it does not read any TVAR before retrying." (~ log)))

    (dohash reads var val
      (let1 actual-val (raw-value-of var)
	(if (eq val actual-val)
	    (progn
	      (log:trace "Tlog ~A listening for tvar ~A changes" (~ log) (~ var))
	      (listen-tvar var log))
	    (progn
	      (log:trace "Tlog ~A: tvar ~A changed, not going to sleep"
			 (~ log) (~ var))
	      (return-from listen-tvars-of nil))))))
  (return-from listen-tvars-of t))


      
  ;; notify-tvar dequeues all logs enqueued on it
  ;; but this log may be enqueued on other tvars that did not change,
  ;; so we must dequeue this log from each tvar waiting list.
  ;; (and since we are at it, we may as well validate the log in the meantime)
  ;; FIXME: dequeue removes the OLDEST element from queue!
  ;; not what we need. use hash-tables for queues, maybe with weak keys?
          

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
	  (setf valid (valid? log))
	  (log:debug "Tlog ~A woke up, is now ~A" (~ log) (if valid "valid" "invalid")))
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

  (when (null (lock-of log))
    (setf (lock-of log) (make-lock (format nil "~A-~A" 'tlog (~ log))))
    (setf (semaphore-of log) (make-condition-variable)))

  ;; we are going to sleep, unless some TVAR changes and/or tells us not to.
  (setf (prevent-sleep-of log) nil)

  (when (listen-tvars-of log)
    (loop while (wait-once log)))

  (log:debug "Tlog ~A not valid: tvars changed while it slept (good), re-executing now" (~ log)))


(defun notify-tlog (log)
  (declare (type tlog log))
  (log:debug "Waking up tlog ~A" (~ log))
  (condition-notify (semaphore-of log)))


;; Copyright (c) 2013, Massimiliano Ghilardi
;; This file is part of STMX.
;;
;; STMX is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; STMX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with STMX. If not, see <http://www.gnu.org/licenses/>.



;; Copyright (c) 2006 Hoan Ton-That
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
