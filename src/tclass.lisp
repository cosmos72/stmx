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

;;;; * Transactional classes

;;;; ** Metaclasses

(defclass transactional-class (standard-class)
  ()
  (:documentation "The metaclass for transactional classes.

Classes defined with this metaclass have extra slot options,
see the class TRANSACTIONAL-DIRECT-SLOT for details."))


(defclass transactional-direct-slot (standard-direct-slot-definition)
  ((transactional :accessor transactional-slot?
                  :initarg :transactional
                  :initform t
                  :type boolean))
  (:documentation "The class for direct slots of transactional classes.

Beyond the normal initargs for standard slots, the following
options can be passed to component slots:

:transactional [ T | NIL ] - Specify whether this slot is transactional.
- If true, all reads and writes will be transactional.
- If not specified, the default is to make a transactional slot."))



(defclass transactional-effective-slot (standard-effective-slot-definition)
  ((transactional :accessor transactional-slot?
                  :initarg :transactional
                  :type boolean))
  (:documentation "The class for effective slots of transactional classes.
Exactly analogous to TRANSACTIONAL-DIRECT-SLOT."))


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
    ;;(declare (ignore slot-name))
    (log:info "class ~A slot ~A: direct slots ~{~A ~}~%"
              class slot-name direct-slots)
    (let ((effective-slot (call-next-method))
          (direct-slots (loop for s in direct-slots
                           when (typep s 'transactional-direct-slot)
                           collect s)))
      (unless (null (cdr direct-slots))
        (error "More than one :transactional specifier"))

      (let* ((direct-slot (car direct-slots))
             (is-tslot? (transactional-slot? direct-slot)))
        (setf (transactional-slot? effective-slot) is-tslot?)

        ;; if slot is transactional, replace its :type <x> with :type tvar
        ;; and set its initfunction to (lambda () (new 'tvar ...))
        (when is-tslot?
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
      ((not (transactional-slot? slot))
       obj)
    
      ;; Return the value inside the TVAR.
      ;; During transactions, reading from tvars is recorded to the current tlog.
      ((or (recording?) (hide-tvars?))
       ($ (the tvar obj)))
    
      (t
       ;; Return the tvar itself.
       (the tvar obj)))))


(declaim (inline slot-raw-tvar))
(defun slot-raw-tvar (class instance slot)
  "Return the raw tvar stored in a transactional slot."
  (without-recording-with-show-tvars
    (slot-value-using-class class instance slot)))

(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))
  (cond
    ((not (transactional-slot? slot))
     (call-next-method))
    
    ((or (recording?) (hide-tvars?))
     ;; Get the tvar from the slot and write inside it.
     ;; During transactions, writing tvars is recorded into the current tlog.
     (let1 var (slot-raw-tvar class instance slot)
       (setf ($ var) value)))
      
    (t
     ;; Set the tvar in the slot
     (call-next-method))))



(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))
  (cond
    ((not (transactional-slot? slot))
     (call-next-method))
    
    ((or (recording?) (hide-tvars?))
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
    ((not (transactional-slot? slot))
     (call-next-method))
    
    ((or (recording?) (hide-tvars?))
     ;; Get the tvar from the slot, and unbind its value.
     ;; During transactions, unbinding the tvar is recorded into the current tlog.
     (unbind-$ (slot-raw-tvar class instance slot)))
    
    (t
     ;; raw access: unbind the slot.
     (call-next-method))))



;;;; ** Inheritance and initialization

(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))



(let1 transactional-object-class (find-class 'transactional-object)
  (defmethod compute-class-precedence-list ((class transactional-class))
    "Add transactional-object as the first superclass of a transactional object
if not already present in the superclass list."
    (let1 superclasses (call-next-method)
      (if (member transactional-object-class superclasses)
	  superclasses
	  `(,(first superclasses) ;; this is the class itself being defined
	     ,transactional-object-class
	     ,@(rest superclasses))))))



(defmethod shared-initialize ((instance transactional-object)
                              slot-names &rest initargs)
  "For every transactional slot, turn its initarg into a tvar."
  (let1 initargs (copy-list initargs)
    (dolist (slot (class-slots (class-of instance)))
      ;; Only check those where `transactional-slot?' is true.
      (when (and (typep slot 'transactional-effective-slot)
                 (transactional-slot? slot))
        (dolist (initarg-name (slot-definition-initargs slot))
          (let1 fragment (rest (member initarg-name initargs))
            (when fragment
              (setf (first fragment) (new 'tvar :value (first fragment))))))))

    ;; Disable recording so that slots initialization is NOT recorded to the log.
    ;; Show-tvars so that (setf slot-value) will set the tvar, not its contents
    (without-recording-with-show-tvars
      (apply #'call-next-method instance slot-names initargs))))
