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

;;;; ** Implementation classes

(declaim (type integer *tlog-id-counter*))
(defvar *tlog-id-counter* 0)

(defclass tlog ()
  ((reads  :accessor reads-of
           :initarg :reads
           :initform (make-hash-table :test 'eq)
           :type hash-table
           :documentation "TVARs read during transaction, mapped to their read value")
   (writes :accessor writes-of
           :initarg :writes
           :initform (make-hash-table :test 'eq)
           :type hash-table
           :documentation "TVARs written during transaction, mapped to their new values")
   (parent :accessor parent-of
           :initarg :parent
           :initform nil
           :type (or null tlog)
           :documentation "Parent of this TLOG. Used for nested transactions")
   (lock   :accessor lock-of
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

  (:documentation "A transaction log (TLOG) is a record of the reads and writes
to transactional memory performed during a transaction.

Transaction logs are automatically populated by reading and writing
transactional objects (TOBJs) or transactional variables (TVARs),
and are later committed to memory if the transaction completes successfully."))



(defvar +unbound+ (gensym "UNBOUND-"))

(defclass tvar ()
  ((value :accessor raw-value-of
          :initarg :value
          :initform +unbound+)
   (lock :accessor lock-of
         :initform (make-lock "TVAR"))
   (waiting :accessor waiting-for
            :initform (make-hash-table :test 'eq)
            :type hash-table)
   (waiting-lock :accessor waiting-lock-of
                 :initform (make-lock "WAITING-LOCK")))

  (:documentation "A transactional variable (TVAR) is the smallest unit
of transactional memory.
It contains a single value that can be read or written during a transaction
using ($ tvar) and (SETF ($ tvar) value).

TVARs are seldom used directly, since transactional objects (TOBJs) wrap them
with a more intuitive and powerful interface: you can read and write normally
the slots of a transactional object (with slot-value, accessors ...),
and behind the scenes the slots will be stored in transactional memory implemented by TVARs."))



;;;; ** Flags to control the behaviour of TLOGs and TOBJs

(declaim (type boolean *record-to-tlogs*))
(defvar *record-to-tlogs* nil
  "A flag indicating if transactions are being recorded to TLOGs or not.

Recording is normally enabled during transactions,
while it is normally disabled in these cases:
- outside transactions
- when initializing TOBJs slots
- during some MOP calls (slot-value-using-class etc.) that implement
  access to TOBJs slots.")

(declaim (inline recording?))
(defun recording? ()
  "Return true if transactions are being recorded to TLOGs."
  *record-to-tlogs*)

(defmacro with-recording (&body body)
  "Enable recording of reads and writes to TLOGs while executing BODY."
  `(let1 *record-to-tlogs* t
     ,@body))


(declaim (type boolean *hide-tvars*))
(defvar *hide-tvars* t
  "A boolean controlling the behaviour of the function (slot-value)
and of slot accessors for TOBJs slots:
- if false, (slot-value) and accessors will get or set the actual TVAR stored
  inside the slot.
- if true, (slot-value) and accessors will get or set the value stored inside
  the slot's TVAR.

This flag is almost always true; it is temporarily set to false during some MOP
calls (slot-value-using-class ...) that implement access to TOBJs slots.")

(defun hide-tvars? ()
  *hide-tvars*)

(defmacro without-recording-with-show-tvars (&body body)
  "Disable recording of transactions to TLOGs and disable hiding TVARs
inside TOBJs slots while executing BODY."
  `(let ((*record-to-tlogs* nil)
         (*hide-tvars* nil))
     ,@body))



;;;; ** Current transaction log

(declaim (type (or null tlog) *tlog*))
(defvar *tlog* nil
  "The current transaction log.")

(declaim (inline current-tlog))
(defun current-tlog ()
  "Return the current transaction log"
  *tlog*)

(defmacro with-recording-to-tlog (log &body body)
  "Use LOG as the current transaction log and enable recording of transactions
to TLOGs while executing BODY."
  `(let1 *tlog* ,log
     (with-recording
       ,@body)))


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
