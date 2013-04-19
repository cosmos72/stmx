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
                  ;; if a transactional class inherits a slot
                  ;; from a non-transactional parent class,
                  ;; by default the slot will remain non-transactional
                  :initform nil
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




#-stmx-have-mop.setf-slot-definition-type
(defmethod slot-definition-type ((effective-slot transactional-effective-slot))
  "Work around the lack of (setf slot-definition-type) in CCL, LispWorks etc."
  (if (transactional-slot? effective-slot)
      t
      (call-next-method)))


;; guard against recursive calls
(defvar *recursive-call-list-classes-containing-direct-slots* nil)

(defun list-classes-containing-direct-slots (direct-slots class)
  (if *recursive-call-list-classes-containing-direct-slots*
      "error listing superclasses, recursive call to listing superclasses"
      (let1 *recursive-call-list-classes-containing-direct-slots* t
        (handler-case
            (loop for superclass in (class-precedence-list class)
               when (intersection direct-slots (class-direct-slots superclass))
               collect (class-name superclass))
          (condition (err)
            (handler-case
                (format nil "error listing superclasses, ~A: ~A" (type-of err) err)
              (condition ()
                "error listing superclasses")))))))

;; guard against recursive calls
(defvar *recursive-call-compute-effective-slot-definition* nil)

(let1 lambda-new-tvar (lambda () (make-tvar))
  (defmethod compute-effective-slot-definition ((class transactional-class)
                                                slot-name direct-slots)
    "If more than one transactional-direct-slot with the same name is present,
ensure that all of them have the same :transactional flag value (all T or all NIL),
otherwise signal an error.

For transactional slots, replace :type ... with :type tvar
and wrap :initform with (lambda () (make-tvar ...)"

    (when (member class *recursive-call-list-classes-containing-direct-slots*)
      (log:warn "recursive call to (compute-effective-slot-definition ~A)" class)
      (return-from compute-effective-slot-definition (call-next-method)))

    (let ((*recursive-call-compute-effective-slot-definition*
           (cons class *recursive-call-compute-effective-slot-definition*))

          (effective-slot (call-next-method))
          (direct-slots (loop for slot in direct-slots
                           when (typep slot 'transactional-direct-slot)
                           collect slot)))

      (when direct-slots
        (loop for tail on direct-slots
           for slot1 = (first tail)
           for slot2 = (second tail)
           while slot2 do
             (unless (eq (transactional-slot? slot1)
                         (transactional-slot? slot2))
               (error "Transactional slot ~A in class ~A is inherited multiple times,
some with :transactional T and some with :transactional NIL.
This is not allowed, please set all the relevant :transactional flags
to the same value. Problematic classes containing slot ~A: ~{~A ~}"
                      slot-name (class-name class) slot-name
                      (list-classes-containing-direct-slots direct-slots class))))
        
        (let* ((direct-slot (first direct-slots))
               (is-tslot? (transactional-slot? direct-slot)))
          (setf (transactional-slot? effective-slot) is-tslot?)

          ;; if slot is transactional remove any type declaration :type <x> because,
          ;; depending on transactional global flags, slot-value and accessors
          ;; will accept/return either a TVAR or the actual type.
          ;; Also set slot initfunction to (lambda () (make-tvar ...))
          (when is-tslot?
            #+stmx-have-mop.setf-slot-definition-type
            ;; in CCL, Lispworks, etc. we specialize the method (slot-definition-type)
            ;; instead - see above
            (when-bind type (slot-definition-type effective-slot)
              (unless (eq t type)
                (setf (slot-definition-type effective-slot) t)))

            (log:trace "class = ~A~%  slot-name = ~A~%  transactional direct-slots = ~A~%  effective-slot = ~A"
                       class slot-name direct-slots effective-slot)
          
            (let* ((direct-initfunction (slot-definition-initfunction direct-slot))
                   (effective-initfunction
                    (if direct-initfunction
                        (lambda () (make-tvar :value (funcall direct-initfunction)))
                        lambda-new-tvar)))

              #+stmx-have-mop.setf-slot-definition-initfunction
              (setf (slot-definition-initfunction effective-slot) effective-initfunction)
              #+lispworks (setf (slot-value effective-slot 'clos::initfunction) effective-initfunction)
              #+ccl (setf (slot-value effective-slot 'ccl::initfunction) effective-initfunction)))))

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



(defun ensure-transactional-object-among-superclasses (direct-superclasses)
  (declare (type list direct-superclasses))
  "Add transactional-object as the first superclass of a transactional class
if not already present in the superclass list."
  (loop for direct-superclass in direct-superclasses do
       (when (typep (find-class direct-superclass) 'transactional-class)
         (return-from ensure-transactional-object-among-superclasses direct-superclasses)))
  
  (cons 'transactional-object
        direct-superclasses))



(defmethod shared-initialize ((instance transactional-object)
                              slot-names &rest initargs)
  "For every transactional slot, turn its initarg into a tvar."

  ;;(log:trace "transactional-object ~A, slot-names = ~A~%  initargs = ~{~A~^ ~}" instance slot-names initargs)

  (let1 initargs (copy-list initargs)
    (dolist (slot (class-slots (class-of instance)))
      ;; Only check those where `transactional-slot?' is true.
      (when (and (typep slot 'transactional-effective-slot)
                 (transactional-slot? slot))
        (dolist (initarg-name (slot-definition-initargs slot))
          (let1 fragment (rest (member initarg-name initargs))
            (when fragment
              (setf (first fragment) (make-tvar :value (first fragment)))
              ;; wrap each initarg only once
              (return))))))

    ;;(log:trace "updated initargs = ~{~A~^ ~}" initargs)
              
    ;; Disable recording so that slots initialization is NOT recorded to the log.
    ;; Show-tvars so that (setf slot-value) will set the tvar, not its contents
    (without-recording-with-show-tvars
      (apply #'call-next-method instance slot-names initargs))))
