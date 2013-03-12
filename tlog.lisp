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
    (let1 actual-val (value-of var)
      (if (eq val actual-val)
          (log:trace "Tvar ~A is up-to-date" var)
          (progn
            (log:trace "Conflict for tvar ~A: expecting ~A, found ~A" var (~ val) (~ actual-val))
            (log:debug "Tlog ~A ..not valid" (~ log))
            (return-from valid? nil)))))
  (log:trace "Tlog ~A ..is  valid" (~ log))
  (return-from valid? t))



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
  (let1 acquired nil
    (log:trace "Tlog ~A committing..." (~ log))
    (unwind-protect
         (progn
           (dohash (writes-of log) var val
             (let1 lock (lock-of var)
               (if (acquire-lock lock nil)
                   (progn
                     (push var acquired)
                     (log:trace "Acquired lock ~A" lock))
                   (progn
                     (log:debug "Tlog ~A ...not committed: could not acquire lock ~A"
                                       (~ log) lock)
                     (return-from commit nil)))))
           (unless (valid? log)
             (log:debug "Tlog ~A ...not committed: log is invalid" (~ log))
             (return-from commit nil))
           (dohash (writes-of log) var val
             (setf (value-of var) val)
             (log:trace "Tvar ~A value updated to ~A, version incremented by 1" var val))
           (log:debug "Tlog ~A ...committed" (~ log))
           (return-from commit t))
      (dolist (var acquired)
        (let1 lock (lock-of var)
          (release-lock lock)
          (log:trace "Released lock ~A" lock)))
      (dolist (var acquired)
        (notify-tvar var)
        (log:trace "Notified threads waiting on ~A" var)))))


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

(defun wait-tlog (log)
  (declare (type tlog log))
  (let1 reads (reads-of log)
    (when (zerop (hash-table-count reads))
      (error "Tried to wait on TLOG ~A, but no TVARs to wait on.~%  This is a BUG either in the STMX library or in the application code!~%  Possible reason is some application code analogous to (atomic (retry))~%  Such code is not allowed and will signal the current error~%  because it does not read any TVAR before retrying." (~ log)))
    (dohash reads var val
      ;; (declare (ignore val))
      (with-slots (waiting waiting-lock) var
        (with-lock-held (waiting-lock)
          (enqueue waiting log))))
    (let1 dummy (make-lock "dummy lock")
      ;; Max: making a new lock for each call to (wait) seems a bit wasteful
      (acquire-lock dummy)
      (condition-wait (semaphore-of log) dummy))))


(defun notify-tlog (log)
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
