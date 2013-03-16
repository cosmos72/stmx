;; -*- lisp -*-

(in-package :stmx)

;;;; ** Implementation classes

(declaim (type integer *tlog-id-counter*))
(defvar *tlog-id-counter* 0)

(defclass tlog ()
  ((reads :accessor reads-of
          :initarg :reads
          :initform (make-hash-table :test #'eq :size 16)
          :type hash-table
          :documentation "Mapping reads done to values")
   (writes :accessor writes-of
           :initarg :writes
           :initform (make-hash-table :test #'eq :size 16)
           :type hash-table
           :documentation "Mapping variables written to new values")
   (lock :accessor lock-of
         :initform nil)
   (semaphore :accessor semaphore-of
	      :initform nil)
   (prevent-sleep :accessor prevent-sleep-of
         :initform nil
	 :type boolean
	 :documentation "Flag to prevent TLOGs from sleeping.
Set by TVARs when they change")
   (id :reader id-of
       :initform (incf *tlog-id-counter*)
       :type integer))

  (:documentation "A transaction log (TLOG) is a record of what
reads and writes have been done.

Transaction logs are written during the execution of a
transaction (by reading and writing transactional objects or TVARs).
Transactions logs are committed to memory by COMMIT later on."))



(defvar +unbound+ (gensym "UNBOUND-"))

(defclass tvar ()
  ((value :accessor raw-value-of
          :initarg :value
          :initform +unbound+)
   (lock :accessor lock-of
         :initform (make-lock "TVAR"))
   (waiting :accessor waiting-for
            :initform (make-hash-table :test 'eq :weakness :key)
            :type hash-table)
   (waiting-lock :accessor waiting-lock-of
                 :initform (make-lock "WAITING-LOCK")))

  (:documentation "A transactional variable (TVAR) holds a value.
See ($ tvar) and (SETF ($ tvar) value) for reading and writing them."))



;;;; ** Recording transactions

(defvar *record-transactions* nil
  "A boolean specifying whether or not transactions are recorded
to log.")

(defun recording? ()
  *record-transactions*)

(defmacro with-recording (&body body)
  "Turn recording of reads and writes on.  Recording is normally
on inside transactions."
  `(let1 *record-transactions* t
     ,@body))

(defmacro without-recording (&body body)
  "Turn recording of reads and writes off.  Recording is normally
off outside transactions (ie at the REPL) and when initializing
transactional objects."
  `(let1 *record-transactions* nil
     ,@body))

;;;; ** Returning contents

(defvar *return-contents* t
  "A boolean specifying whether or not slot accessors should
read/write the contents of the transactional variable instead of the
variable itself.")

(defun returning? ()
  *return-contents*)

(defmacro with-returning (&body body)
  `(let1 *return-contents* t
     ,@body))

(defmacro without-returning (&body body)
  `(let1 *return-contents* nil
     ,@body))

;;;; ** Current transaction log

(defvar *tlog* nil
  "The current transaction log.")

(defun current-tlog ()
  *tlog*)

(defmacro with-tlog (log &body body)
  "Execute BODY with the default transasction log being LOG."
  `(let1 *tlog* ,log
     (with-recording
       ,@body)))

(defmacro with-new-tlog (log &body body)
  "Execute BODY with the default transaction log being a newly
allocated transaction log bound to LOG."
  `(let1 ,log (new 'tlog)
     (with-tlog ,log
       ,@body)))


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
