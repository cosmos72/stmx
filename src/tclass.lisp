;; -*- lisp -*-

(in-package :stmx)

;;;; * Transactional classes

;;;; ** Metaclasses

(defclass transactional-class (standard-class)
  ()
  (:documentation "The metaclass for transactional classes.

Classes defined with this metaclass have extra slot options,
see the class TRANSACTIONAL-DIRECT-SLOT for details."))

(defclass transactional-direct-slot (standard-direct-slot-definition)
  ((transactional :accessor slot-transactional
                  :initarg :transactional
                  :initform t))
  (:documentation "The class for direct slots of transactional classes.

Other than the initargs for standard slots the following
options can be passed to component slots:

:transactional [ T | NIL ] - Specify whether this slot is a
transactional slot and that all reads and writes should be
committed to log. If :transactional [ T | NIL ] is not specified,
the slot is transactional"))

(defclass transactional-effective-slot (standard-effective-slot-definition)
  ((transactional :accessor slot-transactional
                  :initarg :transactional))
  (:documentation "The class for effective slots of transactional
classes.

Exactly like TRANSACTIONAL-EFFECTIVE-SLOT."))



;;;; ** Inheritance

(defmethod validate-superclass ((class transactional-class)
                                (superclass standard-class))
  (declare (ignore class superclass))
  t)

;;;; ** Slot definitions

(let1 transactional-direct-slot-class (find-class 'transactional-direct-slot)
  (defmethod direct-slot-definition-class ((class transactional-class)
                                           &rest initargs)
    (declare (ignore initargs))
    transactional-direct-slot-class))



(let1 transactional-effective-slot-class (find-class 'transactional-effective-slot)
  (defmethod effective-slot-definition-class ((class transactional-class)
                                              &rest initargs)
    (declare (ignore initargs))
    transactional-effective-slot-class))



(let1 lambda-new-tvar (lambda () (new 'tvar))
  (defmethod compute-effective-slot-definition ((class transactional-class)
                                                slot-name direct-slots)
    (declare (ignore slot-name))
    (let ((effective-slot (call-next-method))
          (direct-slots (remove-if-not
			 (lambda (x) (typep x 'transactional-direct-slot))
			 direct-slots)))
      (unless (null (cdr direct-slots))
        (error "More than one :transactional specifier"))

      (let* ((direct-slot (car direct-slots))
             (is-transactional-slot (slot-transactional direct-slot)))
        (setf (slot-transactional effective-slot) is-transactional-slot)

        ;; if slot is transactional, replace its :type <x> with :type tvar
        ;; and set its initfunction to (lambda () (new 'tvar ...))
        (when is-transactional-slot
          (setf (slot-definition-type effective-slot) 'tvar)
          (let* ((direct-initfunction (slot-definition-initfunction direct-slot))
                 (effective-initfunction
                  (if direct-initfunction
                      (lambda () (new 'tvar :value (funcall direct-initfunction)))
                      lambda-new-tvar)))
            (setf (slot-definition-initfunction effective-slot) effective-initfunction))))
      effective-slot)))




;;;; ** Slot access

(defmethod slot-value-using-class ((class transactional-class) instance
                                   (slot transactional-effective-slot))
  (declare (ignore instance))
  
  ;; Get the TVAR from the slot
  (let1 obj (call-next-method)
    (cond
      ((not (slot-transactional slot))
       obj)
    
      ;; Return the value inside the TVAR.
      ;; During transactions, reading of the tvar (which is found with
      ;; `call-next-method') is recorded to the current tlog.
      ((or (recording?) (returning?))
       ($ (the tvar obj)))
    
      (t
       ;; Return the tvar itself.
       (the tvar obj)))))


(defun slot-raw-tvar (class instance slot)
  "Return the raw tvar stored in a transactional slot"
  (without-recording
    (without-returning
      (slot-value-using-class class instance slot))))

(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))
  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((or (recording?) (returning?))
     ;; Get the tvar from the slot and write inside it.
     ;; During transactions, writing of the tvar is recorded into the current tlog.
     (let1 var (slot-raw-tvar class instance slot)
       (setf ($ var) value)))
      
    (t
     ;; Set the tvar in the slot
     (call-next-method))))



(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))
  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((or (recording?) (returning?))
     ;; Get the tvar from the slot, and return true if it is bound to a value.
     ;; During transactions, the checking whether the tvar is bound
     ;; is recorded to the current tlog.
     (bound-$? (slot-raw-tvar class instance slot)))

    (t
     ;; Raw access: check if the slot itself is bound
     (call-next-method))))


(defmethod slot-makunbound-using-class ((class transactional-class) instance
                                        (slot transactional-effective-slot))
  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((or (recording?) (returning?))
     ;; Get the tvar from the slot, and unbind its value.
     ;; During transactions, unbinding the tvar is recorded into the current tlog.
     (unbind-$ (slot-raw-tvar class instance slot)))
    
    (t
     ;; raw access: unbind the slot.
     (call-next-method))))


(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))




;;;; ** Defining


(let1 transactional-object-class (find-class 'transactional-object)
  (defmethod compute-class-precedence-list ((class transactional-class))
    ;; add transactional-object as the first superclass of a transactional object
    ;; if not already present in the superclass list
    (let1 superclasses (call-next-method)
      (if (member transactional-object-class superclasses)
	  superclasses
	  `(,(first superclasses) ;; this is the class itself being defined
	     ,transactional-object-class
	     ,@(rest superclasses))))))



;;;; ** Initializing

(defmethod shared-initialize ((instance transactional-object)
                              slot-names &rest initargs)
  ;; For every transactional slot we turn its initarg into a tvar.
  (let1 initargs (copy-list initargs)
    (dolist (slot (class-slots (class-of instance)))
      ;; Only check those where `slot-transactional' is true.
      (when (and (typep slot 'transactional-effective-slot)
                 (slot-transactional slot))
        (dolist (initarg-name (slot-definition-initargs slot))
          (let1 fragment (rest (member initarg-name initargs))
            (when fragment
              (setf (first fragment) (new 'tvar :value (first fragment))))))))

    ;; We turn off recording in the initialization so that any slot
    ;; changes are NOT recorded to the log.
    ;; We turn off returning so that (setf slot-value) will set the tvar, not its contents
    (without-recording
      (without-returning
        (apply #'call-next-method instance slot-names initargs)))))



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
