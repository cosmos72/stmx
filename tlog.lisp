;; -*- lisp -*-

(in-package :cl-stm2)

;;;; * Transaction logs

;;;; ** Utilities

(defmacro dohash (hash key value &body body)
  "execute body on each key/value pair contained in hash table"
  `(loop for ,key being each hash-key in ,hash
        using (hash-value ,value)
        do (progn ,@body)))
        

;;;; ** Committing

(defun commit (log)
  "Commit a LOG to memory.

It returns a boolean specifying whether or not the transaction
log was committed.  If the transaction log couldn't be committed
it probably means that another transaction log that writes the
same variables is being committed."

  (declare (type tlog log))
  (let ((acquired nil)
        (id (tlog-id log)))
    (stm.commit.dribble "Tlog ~A committing..." id)
    (unwind-protect
         (progn
           (dohash (writes-of log) var val
             (let1 lock (lock-of var)
               (if (acquire-lock lock nil)
                   (progn
                     (push var acquired)
                     (stm.commit.dribble "Acquired lock ~A" lock))
                   (progn
                     (stm.commit.debug "Tlog ~A ...not committed: could not acquire lock ~A"
                                       id lock)
                     (return-from commit nil)))))
           (unless (check? log)
             (stm.commit.debug "Tlog ~A ...not committed: log is invalid" id)
             (return-from commit nil))
           (dohash (writes-of log) var val
             (setf (value-of var) val)
             (stm.commit.dribble "Tvar ~A value updated to ~A, version incremented by 1" var val))
           (stm.commit.debug "Tlog ~A ...committed" id)
           (return-from commit t))
      (dolist (var acquired)
        (let1 lock (lock-of var)
          (release-lock lock)
          (stm.commit.dribble "Released lock ~A" lock)))
      (dolist (var acquired)
        (unwait-tvar var)
        (stm.commit.dribble "Notified threads waiting on ~A" var)))))


(defun check? (log)
  (declare (type tlog log))
  (let1 id (tlog-id log)
    (stm.check.dribble "Tlog ~A checking.." id)
    (dohash (reads-of log) var ver
      (let1 actual-ver (version-of var)
        (if (= ver actual-ver)
            (stm.check.dribble "Version ~A is valid" ver)
            (progn
              (stm.check.dribble "Version ~A doesn't match ~A" ver actual-ver)
              (stm.check.debug   "Tlog ~A ..invalid" id)
              (return-from check? nil)))))
    (stm.check.dribble "Tlog ~A ..valid" id)
    (return-from check? t)))

;;;; ** Merging

(defun merge-tlogs (log1 log2)
  (declare (type tlog log1 log2))
  (dohash (writes-of log2) var val
    (setf (gethash var (writes-of log1)) val))
  (dohash (reads-of log2) var ver
    (setf (gethash var (reads-of log1))
          (max ver
               ;; Max: needed? I expected log2 to be *always* more up-to-date...
               (aif2 (gethash var (reads-of log1))
                     it
                     0))))
  log1)

;;;; ** Waiting

(defun wait-tlog (log)
  (declare (type tlog log))
  (let1 reads (reads-of log)
    (when (zerop (hash-table-count reads))
      (error "Tried to wait on tlog ~A, but no tvars to wait on.~%  This is a BUG either in the STM library or in the application code!~%  Possible reason: code analogous to (atomic (retry)) is invalid~%  and will signal the current error because it does not read any tvar~%  before retrying." (tlog-id log)))
    (dohash reads var val
      ;; (declare (ignore val))
      (with-slots (waiting waiting-lock) var
        (with-lock-held (waiting-lock)
          (enqueue waiting log))))
    (let1 dummy (make-lock "dummy lock")
      ;; Max: making a new lock for each call to (wait) seems a bit wasteful
      (acquire-lock dummy)
      (condition-wait (semaphore-of log) dummy))))


(defun unwait-tlog (log)
  (declare (type tlog log))
  (condition-notify (semaphore-of log)))

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
