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

(enable-#?-syntax)

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

(defmethod compute-effective-slot-definition ((class transactional-class)
                                              slot-name direct-slots)
  "If more than one transactional-direct-slot with the same name is present,
ensure that all of them have the same :transactional flag value (all T or all NIL),
otherwise signal an error."

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
        (setf (transactional-slot? effective-slot) is-tslot?)))

    effective-slot))




;;;; ** Transactional objects definition

(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))



(defun ensure-transactional-object-among-superclasses (direct-superclasses)
  "Add transactional-object as the first superclass of a transactional class
if not already present in the superclass list."
  (declare (type list direct-superclasses))
  (loop for direct-superclass in direct-superclasses do
       (when (typep (find-class direct-superclass) 'transactional-class)
         (return-from ensure-transactional-object-among-superclasses direct-superclasses)))
  (cons 'transactional-object
        direct-superclasses))


(defun list-direct-slot-in-superclasses (slot-name class-precedence-list)
  "List the direct slots named SLOT-NAME found in class precedence list."
  (declare (type symbol slot-name)
           (type list class-precedence-list))
  (let1 matching-slots nil
    (loop for superclass in class-precedence-list
       for slots = (class-direct-slots superclass) do
         (dolist (slot slots)
           (when (eq slot-name (slot-definition-name slot))
             (push slot matching-slots))))
    (nreverse matching-slots)))
           

(defun transactional-slot-definition? (slot-definition)
  "Direct slots defined in transactional classes are transactional themselves,
UNLESS explicitly defined with the option :transactional nil."
  (let1 transactional-opt (member :transactional slot-definition)
    ;; return T unless we find :transactional nil
    (or (null transactional-opt)
        (not (null (second transactional-opt))))))

(defun typespec-allows? (type typespec)
  "Return T if TYPESPEC is compatible with TYPE, i.e. if:
* TYPESPEC is equal to TYPE
* TYPESPEC is equal to (OR ... TYPESPEC-I ...) and at least one TYPESPEC-I is compatible with TYPE
* TYPESPEC is equal to (AND ... TYPESPEC-I ...) and all TYPESPEC-I are compatible with TYPE.
Otherwise return NIL."
  (and
   (or
    (equal type typespec)
    (when (consp typespec)
      (case (first typespec)
        (or (loop for typespec-i in (rest typespec)
                thereis (typespec-allows? type typespec-i)))
        (and (loop for typespec-i in (rest typespec)
                always (typespec-allows? type typespec-i)))
        (otherwise nil))))
   t))

(defun adjust-transactional-slot-definition (slot-definition
                                             class-name class-precedence-list)
  "Adjust a single slot definition for a transactional class: unless the slot
is defined as :transactional NIL, wrap its :initform with a TVAR and alter its
:type to also accept TVARs"
  (declare (type list slot-definition class-precedence-list)
           (type symbol class-name))

  (let1 slot-name (first slot-definition)

    (unless (transactional-slot-definition? slot-definition)
      ;; not transactional, keep the slot definition intact
      (return-from adjust-transactional-slot-definition slot-definition))

    ;; transactional, collect the slot type and initform from the superslots
    (let* ((plist (rest slot-definition))

           (initform-tx? nil) ;; slot initform is already TVAR-aware?
           (initform? (member :initform plist))
           (initform (second initform?))

           (type? (member :type plist))
           (type (second type?))

           (super-type-tx? nil) ;; super-slot type is already TVAR-aware?
           (super-type? nil)
           (super-type nil)

           (super-slots (list-direct-slot-in-superclasses slot-name class-precedence-list)))

      ;; find the most specific :initform and :type
      (loop until (and super-type? initform?)
         for slot in super-slots
         for tx? = (and (typep slot 'transactional-direct-slot) (transactional-slot? slot))
         do
           (when (and (not initform?) (slot-definition-initfunction slot))
             (setf initform-tx? tx?
                   initform? t
                   initform (slot-definition-initform slot)))
           
           (unless super-type?
             (setf super-type-tx? tx?
                   super-type? (slot-definition-type slot)
                   super-type super-type?)))

      ;; if super-slot :initform and/or :type are already TVAR-aware,
      ;; keep things simple and don't repeat them

      (unless initform-tx?
        (setf (getf plist :initform) (if initform? `(tvar ,initform) '(tvar))))

      ;; CLOS allows to add type restrictions to a derived slot type,
      ;; but forbids adding alternatives... we cannot add TVARs as allowed :type,
      ;; the only hope is for them to be already allowed
      
      (unless (or super-type-tx? (member super-type '(t nil))
                  (typespec-allows? 'tvar super-type))
        (warn "transactional class ~A defines transactional slot ~A to override
a non-transactional superclass slot with the same name,
but the superslot already contains a type definition ~A.
STMX cannot add a type exception to also store TVARs in slot ~A,
expect type errors when instantiating ~A!

Solution: remove the type definition from superslot, or at least explicitly
add TVARs as one of the slot allowed types.
"
              class-name slot-name super-type slot-name class-name))

      (when type?
        (setf (getf plist :type) (if (or (eq nil type) (eq t type)) t `(or ,type tvar))))
      
      (cons slot-name plist))))
    

(defun adjust-transactional-slots-definitions (slots-definitions
                                               class-name direct-superclasses-names)
  "Adjust each slot definition for a transactional class:
unless a slot is defined as :transactional NIL, wrap its :initform with a TVAR
and alter its :type to also accept TVARs"
  (declare (type list direct-superclasses-names slots-definitions)
           (type symbol class-name))

  (let1 class-precedence-list
      (clos-compute-class-precedence-list class-name
                                          direct-superclasses-names)

    (loop for class in class-precedence-list do
         (ensure-finalized class))

    (loop for slot-definition in slots-definitions
       collect (adjust-transactional-slot-definition slot-definition
                                                     class-name class-precedence-list))))



(defmacro transactional ((defclass class-name direct-superclasses direct-slots &rest class-options))
  "Define CLASS-NAME as a new transactional class.
Use this macro to wrap a normal DEFCLASS as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))

The effect is the same as DEFCLASS, plus:
- by default, slots are transactional memory (implemented by TVARs)
- it inherits also from TRANSACTIONAL-OBJECT
- the metaclass is TRANSACTIONAL-CLASS"
  `(,defclass ,class-name ,(ensure-transactional-object-among-superclasses direct-superclasses)
     ,(adjust-transactional-slots-definitions direct-slots class-name direct-superclasses)
     ,@class-options
     #?+disable-optimize-slot-access (:optimize-slot-access nil)
     (:metaclass transactional-class)))



;;;; ** Signalling unbound slots

(defun unbound-slot-error (class instance slot)
  "Signal an unbound-slot error and allow the user to continue by specifying or storing a value."
  (declare (type transactional-class class)
           (type transactional-object instance)
           (type transactional-effective-slot slot))

  (restart-case (error 'unbound-slot :instance instance :name (slot-definition-name slot))
    (use-value (value)
      :report "Specify a value to return as the slot-value."
      :interactive (lambda ()
                     (format t "~&Value to return: ")
                     (list (eval (read))))
      value)
    (store-value (value)
      :report "Specify a value to store and return as the slot-value ."
      :interactive (lambda ()
                     (format t "~&Value to store and return: ")
                     (list (eval (read))))
      (setf (slot-value-using-class class instance slot) value))))


;;;; ** Slot access


(defmethod slot-value-using-class ((class transactional-class) instance
                                   (slot transactional-effective-slot))
  (declare (ignore class instance))
  
  ;; Get the slot value - it may be a TVAR.
  (let1 obj (call-next-method)

    (if (and (transactional-slot? slot)
             (or (recording?) (hide-tvars?)))

        ;; It is a TVAR, return its value.
        ;; During transactions, reading from tvars is recorded to the current tlog.
        (multiple-value-bind (value bound?) (peek-$ (the tvar obj))
          (if bound?
              value
              (unbound-slot-error class instance slot)))
    
        ;; Return the plain slot-value.
        obj)))


(declaim (inline slot-raw-tvar))
(defun slot-raw-tvar (class instance slot)
  "Return the raw tvar stored in a transactional slot."
  (without-recording-with-show-tvars
    (slot-value-using-class class instance slot)))

(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))

  (if (and (transactional-slot? slot)
           (or (recording?) (hide-tvars?)))
    
      ;; Get the tvar from the slot and write inside it.
      ;; During transactions, writing tvars is recorded into the current tlog.
      (let1 var (slot-raw-tvar class instance slot)
        (setf ($-slot var) value))
      
      ;; Write in the slot
      (call-next-method)))



(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))

  (if (and (transactional-slot? slot)
           (or (recording?) (hide-tvars?)))

     ;; Get the tvar from the slot, and return true if it is bound to a value.
     ;; During transactions, the checking whether the tvar is bound
     ;; is recorded to the current tlog.
     (bound-$? (slot-raw-tvar class instance slot))

     ;; Raw access: check if the slot itself is bound
     (call-next-method)))


(defmethod slot-makunbound-using-class ((class transactional-class) instance
                                        (slot transactional-effective-slot))
  (if (and (transactional-slot? slot)
           (or (recording?) (hide-tvars?)))

      ;; Get the tvar from the slot, and unbind its value.
      ;; During transactions, unbinding the tvar is recorded into the current tlog.
      (unbind-$ (slot-raw-tvar class instance slot))
    
      ;; raw access: unbind the slot.
      (call-next-method)))




;;;; ** Transactional objects initialization


(defmethod shared-initialize ((instance transactional-object)
                              slot-names &rest initargs)
  "For every transactional slot, wrap its initarg into a tvar."

  ;;(log.trace "transactional-object ~A, slot-names = ~A~%  initargs = ~{~A~^ ~}" instance slot-names initargs)

  (let1 initargs (copy-list initargs)
    (dolist (slot (class-slots (class-of instance)))
      ;; Only check those where `transactional-slot?' is true.
      (when (and (typep slot 'transactional-effective-slot)
                 (transactional-slot? slot))
        (dolist (initarg-name (slot-definition-initargs slot))
          (let1 fragment (rest (member initarg-name initargs))
            (when fragment
              (setf (first fragment) (tvar (first fragment)))
              ;; wrap each initarg only once
              (return))))))

    ;;(log.trace "updated initargs = ~{~A~^ ~}" initargs)
              
    ;; Disable recording so that slots initialization is NOT recorded to the log.
    ;; Show-tvars so that (setf slot-value) will set the tvar, not its contents
    (without-recording-with-show-tvars
      (apply #'call-next-method instance slot-names initargs))))
