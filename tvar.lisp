;; -*- lisp -*-

(in-package :stmx)

;;;; ** Transactional variables

;;;; ** Signalling unbound variables

(defun unbound-tvar-error (var)
  "Signal an unbound-slot error and allow the user to continue by specifying or storing a value."
  (declare (type tvar var))
  (restart-case (error 'unbound-slot :instance var :name 'value)
    (use-value (value)
      :report "Specify a value to use."
      :interactive (lambda ()
                     (format t "~&Value to use: ")
                     (list (eval (read))))
      value)
    (store-value (value)
      :report "Specify a value to use and store."
      :interactive (lambda ()
                     (format t "~&Value to use and store: ")
                     (list (eval (read))))
      (setf ($ var) value)
      value)))

  

;;;; ** Reading and Writing

(defun read-tvar (var &optional (log (current-tlog)))
  "Record the reading of VAR to LOG.

READ-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

READ-TVAR is just the plumbing behind taking the SLOT-VALUE of a
transactional slot.  Just use readers and accessors as you
normally would on transactional objects, or, if you *really* want
to use TVARs directly, use ($ VAR)"
  (declare (type tvar var)
           (type tlog log))
  (aif2 (gethash var (writes-of log))
        it
        (setf (gethash var (reads-of log)) (raw-value-of var))))


(defun write-tvar (var value &optional (log (current-tlog)))
  "Record the writing of VALUE to VAR to LOG and return VALUE.

WRITE-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

WRITE-TVAR is just the plumbing behind SETF'ing a transactional
slot.  Just use SETF as you normally would on transactional objects or, 
if you *really* want to use TVARs directly, use (SETF ($ VAR) VALUE)"
  (declare (type tvar var)
           (type tlog log))
  (setf (gethash var (writes-of log)) value))



(defun $ (var)
    "Get the value from the transactional variable VAR.
Works both outside and inside transactions.

During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (let1 value (if (recording?)
                  (read-tvar var)
                  (raw-value-of var))
    (if (eq value +unbound+)
        (unbound-tvar-error var)
        value)))


(defun (setf $) (value var)
    "Store VALUE inside transactional variable VAR.
Works both outside and inside transactions.

During transactions, it uses transaction log to record the value."
  (declare (type tvar var))

  (if (recording?)
      (write-tvar var value)
      (setf (raw-value-of var) value)))
               
            
  
(defun bound-$? (var)
    "Return true if transactional variable VAR is bound to a value.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the read
and to check for any value stored in the log."
  (declare (type tvar var))

  (not (eq +unbound+
           (if (recording?)
               (read-tvar var)
               (raw-value-of var)))))


(defun unbind-$ (var)
    "Unbind the value inside transactional variable VAR.
Works both outside and inside transactions.
    
During transactions, it uses transaction log to record the 'unbound' value."
  (declare (type tvar var))

  (if (recording?)
      (write-tvar var +unbound+)
      (setf (raw-value-of var) +unbound+))
  var)


;;;; ** Accessors


(defmethod value-of ((var tvar))
  "Return the value inside a TVAR. Works both outside and inside transactions."
  ($ var))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a TVAR. Works both outside and inside transactions."
  (setf ($ var) value))



;;;; ** Waiting

(defun notify-tvar (var)
  "NOTIFY-TVAR causes all threads waiting for VAR to
change to wake up."
  (declare (type tvar var))
  (with-slots (waiting waiting-lock) var
    (with-lock-held (waiting-lock)
      (until (queue-empty-p waiting)
        (notify-tlog (dequeue waiting))))))


;;;; ** Printing

(defprint-object (obj tvar)
  (format t "~A"
          (if (bound-$? obj)
              ($ obj)
              "<unbound>")))


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
