;; -*- lisp -*-

(in-package :cl-stm2)

;;;; ** Transactional variables

(defmethod initialize-instance :after ((var tvar) &key (value nil value?) &allow-other-keys)
  (setf (slot-value var 'vbox)
        (new-vbox :value (if value? value +unbound+))))
  

;;;; ** Accessors

(declaim (inline version-of value-of-unmasked))

(defun version-of (var)
  "Return the version of a tvar versioned box"
  (declare (type tvar var))
  (vbox-version (vbox-of var)))

(defun value-of-unmasked (var)
  "Return the raw, unmasked value inside a tvar versioned box"
  (declare (type tvar var))
  (vbox-value (vbox-of var)))
  
(defun value-of-unbound-error (var)
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
      ;; this will also increase the vbox version - we want that!
      (setf (value-of var) value)
      value)))

  
(defmethod value-of ((var tvar))
  "Return the value inside a tvar versioned box."
  (let1 value (value-of-unmasked var)
    (if (eq value +unbound+)
        (value-of-unbound-error var)
        value)))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a tvar versioned box. Also increases the version by 1."
  (let1 old-version (version-of var)
    (setf (vbox-of var) (new-vbox
                         :version (1+ old-version)
                         :value value)))
  value)

(defun tvar-bound? (var)
  "Return true if the tvar versioned box is bound to a value."
  (declare (type tvar var))
  (vbox-bound? (vbox-of var)))

(defun tvar-unbind (var)
  "Restore the tvar versioned box to the unbound state."
  (declare (type tvar var))
  (setf (value-of var) +unbound+)
  var)


  


;;;; ** Printing

(defprint-object (obj tvar)
  (let1 box (vbox-of obj)
    (format t "v~A ~A"
            (vbox-version box)
            (if (eq (vbox-value box) +unbound+)
                "<unbound>"
                (vbox-value box)))))

;;;; ** Reading and Writing

(defun read-tvar (var &optional (log (current-tlog)))
  "Record the reading of VAR to LOG.

READ-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

READ-TVAR is just the plumbing behind taking the SLOT-VALUE of a
transactional slot.  Just use readers and accessors as you
normally would on transactional objects."
  (declare (type tvar var)
           (type tlog log))
  (aif2 (gethash var (writes-of log))
        it
        ;; read atomically the tvar value AND version.
        ;; this is the reason we wrap them in an immutable vbox:
        ;; accessing two different tvar slots, value and version,
        ;; would NOT be atomic.
        (let1 box (vbox-of var)
          (setf (gethash var (reads-of log)) (vbox-version box))
          (vbox-value box))))


(defun write-tvar (var val &optional (log (current-tlog)))
  "Record the writing of VAL to VAR to LOG and return VAL.

WRITE-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

WRITE-TVAR is just the plumbing behind SETF'ing a transactional
slot.  Just use SETF as you normally would on transactional
objects."
  (declare (type tvar var)
           (type tlog log))
  (setf (gethash var (writes-of log)) val))


;;;; ** Waiting

(defun unwait-tvar (var)
  "UNWAIT-TVAR causes all threads waiting for VAR to
change to wake up."
  (declare (type tvar var))
  (with-slots (waiting waiting-lock) var
    (with-lock-held (waiting-lock)
      (until (queue-empty-p waiting)
        (unwait-tlog (dequeue waiting))))))



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
