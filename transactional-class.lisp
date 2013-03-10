;; -*- lisp -*-

(in-package :cl-stm2)

(eval-always
  (enable-pf-reader))

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
          (direct-slots (remove-if-not [typep _ 'transactional-direct-slot] direct-slots)))
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

;;(defvar *getf-depth* 0)
(defmethod slot-value-using-class ((class transactional-class) instance
                                   (slot transactional-effective-slot))
  (declare (ignore instance))

  ;;(let ((*getf-depth* (1+ *getf-depth*)))
    ;;(format t "slot-value-using-class (~S ~S ~S)~%" class instance slot)
    ;;(format t "getf-depth ~S slot-transactional ~S recording? ~S returning? ~S~%" *getf-depth* (slot-transactional slot) (recording?) (returning?))
    ;;(cerror "Continue" "stacktrace for debug purposes")

  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((recording?)
     ;; Record the reading of the tvar (which is found with
     ;; `call-next-method') to the current tlog.  We turn off
     ;; recording and returning because we want the actual tvar,
     ;; not the value inside it.
     (read-tvar (call-next-method)))
    
    ((returning?)
     ;; Return the value inside the tvar.
     (value-of (the tvar (call-next-method))))
    
    (t
     ;; Return the tvar itself.
     (call-next-method))))

(defun slot-raw-tvar (class instance slot)
  "Return the raw tvar stored in a transactional slot"
  (without-recording
    (without-returning
      (slot-value-using-class class instance slot))))

;;(defvar *setf-depth* 0)
(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))
  ;;(let ((*setf-depth* (1+ *setf-depth*)))
    ;;(format t "(setf slot-value-using-class) (~S ~S ~S ~S)~%" value class instance slot)
    ;;(format t "setf-depth ~S slot-transactional ~S recording? ~S returning? ~S~%" *setf-depth* (slot-transactional slot) (recording?) (returning?))
    ;;(cerror "Continue" "stacktrace for debug purposes")
    
    (cond
      ((not (slot-transactional slot))
       (call-next-method))
    
      ((recording?)
       ;; Record the writing of the tvar to the current tlog.
       ;; We get the tvar from the slot  and write it in the current tlog
       ;; together with the new value
       (let1 var (slot-raw-tvar class instance slot)
         (write-tvar var value)))
      
      ((returning?)
       ;; Get the tvar from the slot and write inside it
       (let1 var (slot-raw-tvar class instance slot)
         (setf (value-of var) value)))
      (t
       ;; raw access: set the tvar in the slot
       (call-next-method))))



(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))
  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((recording?)
     (error "slot-boundp not supported during transactions, ~S in ~S" slot instance))

    ((returning?)
     ;; check if the tvar has a bound value.
     (boundp-tvar (slot-raw-tvar class instance slot)))
    
    (t
     ;; raw access: check if the slot is bound to a tvar
     (call-next-method))))


(defmethod slot-makunbound-using-class ((class transactional-class) instance
                                        (slot transactional-effective-slot))
  (cond
    ((not (slot-transactional slot))
     (call-next-method))
    
    ((recording?)
     (error "slot-makunbound not supported during transactions, ~S in ~S" slot instance))

    ((returning?)
     ;; keep the slot bound to the tvar, only unbind the tvar contents
     (makunbound-tvar (slot-raw-tvar class instance slot)))
    
    (t
     ;; raw access: unbind the slot.
     (call-next-method))))


(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))

;;;; ** Defining


(defmacro transactional ((defclass class direct-superclasses direct-slots &rest class-options))
  "Define a new transactional class called CLASS.

use this macro to wrap a normal defclass as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))
the effect is the same as DEFCLASS, plus the default metaclass is
TRANSACTIONAL-CLASS, slots are transactional by default, and it inherits
from TRANSACTIONAL-OBJECT by default."
;  (let1 direct-superclasses (or direct-superclasses '(transactional-object))
    `(eval-always
       (,defclass ,class ,direct-superclasses
         ,direct-slots
         ,@class-options
         (:metaclass transactional-class))))


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
