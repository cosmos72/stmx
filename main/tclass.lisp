;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
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


(defgeneric transactional-slot? (slot))

(defclass transactional-direct-slot (standard-direct-slot-definition)
  ((transactional :type boolean :initform t
                  :initarg :transactional :accessor transactional-slot?))
  (:documentation "The class for direct slots of transactional classes.

Beyond the normal initargs for standard slots, the following
options can be passed to component slots:

:transactional [ T | NIL ] - Specify whether this slot is transactional.
- If true, all reads and writes will be transactional.
- If not specified, the default is to make a transactional slot."))

(defclass transactional-effective-slot (standard-effective-slot-definition)
  ((transactional :type boolean :initform nil
                  ;; if a transactional class inherits a slot
                  ;; from a non-transactional parent class,
                  ;; by default the slot will remain non-transactional
                  :initarg :transactional :accessor transactional-slot?))
  (:documentation "The class for effective slots of transactional classes.
Exactly analogous to TRANSACTIONAL-DIRECT-SLOT."))


(defmethod transactional-slot? ((slot slot-definition))
  "Generic implementation for SLOT-DEFINITION. Return NIL"
  nil)



(defmethod validate-superclass ((class transactional-class)
                                (superclass standard-class))
  (declare (ignore class superclass))
  t)


(defun find-slot-by-name (slot-name slots)
  (declare (type symbol slot-name)
           (type list slots))
  (dolist (slot slots)
    (when (eq slot-name (slot-definition-name slot))
      (return slot))))


;;;; ** Transactional slots definitions

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
  "Compose DIRECT-SLOTS coming from CLASS and all is superclasses into a single EFFECTIVE-SLOT.

If more than one transactional-direct-slot with the same name is present,
ensure that all of them have the same :transactional flag value (all T or all NIL),
otherwise signal an error."

  (when (member class *recursive-call-list-classes-containing-direct-slots* :test 'eq)
    (log:warn "recursive call to (compute-effective-slot-definition ~S)" class)
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

             (compile-error "Error compiling TRANSACTIONAL-CLASS ~S
Slot ~S in class ~S is inherited multiple times,
some with :TRANSACTIONAL T and some with :TRANSACTIONAL NIL.
This is not allowed, please set all the relevant :TRANSACTIONAL flags
to the same value. Problematic classes containing slot ~S: ~{~S ~}"
                    (class-name class) slot-name (class-name class) slot-name
                    (list-classes-containing-direct-slots direct-slots class))))
        
      (let* ((direct-slot (first direct-slots))
             (is-tslot? (transactional-slot? direct-slot)))

        (setf (transactional-slot? effective-slot) is-tslot?)))
    
    effective-slot))



;;;; ** Transactional object

(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))




;;;; ** Utilities

(defmacro tvar-wrap-slot-macro (object slot-name)
  (with-gensym obj
    `(let ((,obj ,object))
       (setf (slot-value ,obj ,slot-name)
             (tvar
              (if (slot-boundp ,obj ,slot-name)
                  (slot-value ,obj ,slot-name)
                  +unbound-tvar+))))))


(defmacro tvar-unwrap-slot-macro (object slot-name)
  (with-gensyms (obj value)
    `(let ((,obj ,object))
       (when (slot-boundp ,obj ,slot-name) ;; paranoia
         (let ((,value ($ (the tvar (slot-value ,obj ,slot-name)))))
           (if (eq ,value +unbound-tvar+)
               (slot-makunbound ,obj ,slot-name)
               (setf (slot-value ,obj ,slot-name) ,value)))))))


(declaim (inline tvar-wrap-slot tvar-unwrap-slot))

(defun tvar-wrap-slot (obj slot-name)
  ;; CMUCL does not like
  ;; (declare (type transactional-object obj))
  ;; when this function is expanded inline in
  ;; (update-instance-for-redefined-class)
  (declare (type standard-object obj)
           (type symbol slot-name))

  (tvar-wrap-slot-macro obj slot-name))


(defun tvar-unwrap-slot (obj slot-name)
  (declare (type standard-object obj)
           (type symbol slot-name))

  (tvar-unwrap-slot-macro obj slot-name))


(declaim (inline find-class-or-nil))

(defun find-class-or-nil (class-name)
  (find-class class-name nil))


(defun ensure-transactional-object-among-superclasses (direct-superclasses)
  "Add TRANSACTIONAL-OBJECT as the first superclass of a transactional class
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

  (loop for superclass in class-precedence-list
     for slots = (class-direct-slots superclass)
     for slot = (find-slot-by-name slot-name slots)
     when slot
     collect slot))
           

(defun slot-form-transactional? (slot-form)
  "Does SLOT-FORM define a transactional slot?
Direct slots defined in transactional classes are by default transactional,
UNLESS explicitly defined with the option :transactional nil."

  (declare (type (or symbol list) slot-form))
  (block nil
    (when (symbolp slot-form)
      (return t))
      
    (let ((tx-opt-and-rest (member :transactional slot-form)))
      ;; return T unless we find :transactional nil
      (unless tx-opt-and-rest
        (return t))
      
      (let ((tx-opt (second tx-opt-and-rest)))
        (when (member tx-opt '(nil t))
          (return tx-opt))

        (compile-error "Error compiling TRANSACTIONAL-CLASS slot ~S:
Unsupported :TRANSACTIONAL flag ~S, expecting either T or NIL"
                       (first slot-form) tx-opt)))))
                    


(defun slot-form-name (slot-form)
  (declare (type (or symbol list) slot-form))
  (if (symbolp slot-form)
      slot-form
      (first slot-form)))

(defun find-slot-form-by-name (slot-name slot-forms)
  (declare (type symbol slot-name)
           (type list slot-forms))
  (dolist (slot-form slot-forms)
    (when (eq slot-name (slot-form-name slot-form))
      (return slot-form))))


#-(and)
(defun update-slot-form (slot-form key value)
  "non-destructively associate KEY to VALUE in SLOT-FORM.
Return modified SLOT-FORM."
  (declare (type keyword key))
  (when (symbolp slot-form)
    (return-from update-slot-form (list slot-form key value)))
  (let ((temp (list (pop slot-form)))
        (replaced nil))
    (loop while slot-form
       do
         (let* ((k (pop slot-form))
                (v (pop slot-form)))
           (push k temp)
           (if (eq k key)
               (progn
                 (setf replaced t)
                 (push value temp))
               (push v temp))))

    (unless replaced
      (push key temp)
      (push value temp))
    (nreverse temp)))
    
(defun copy-slot-form (slot-form)
  (declare (type (or symbol list) slot-form))
  (if (symbolp slot-form)
      (list slot-form)
      (copy-list slot-form)))
  

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

;;;; ** functions to manipulate the slot forms inside (defclass ...)



(defun error-tclass-slot-name (class-name slot-name)
  (declare (type symbol class-name slot-name))
  (compile-error "Error compiling TRANSACTIONAL-CLASS ~S.
Unsupported slot name ~S" class-name slot-name)) 


(defun cerror-tclass-slot-change (class-name slot-name option old-value new-value)
  (declare (type symbol class-name slot-name))
  
  (unless (eq old-value new-value)
    (compile-cerror
     "Continue anyway."
     "Error compiling TRANSACTIONAL-CLASS ~S.
Unsupported redefinition of class ~S slot ~S from ~S ~S to ~S ~S.
Possible solutions:
1) Continue anyway. Existing istances of class ~S and its subclasses
   will no longer work properly and should be discarded.
   You can also avoid this error by uninterning ~S before redefining it.
2) Abort and use a workaround to preserve existing instances.
   Possible workarounds include:
   a) if the slot is not inherited from a superclass,
      you can remove the slot then re-add it.
   b) if the slot is inherited from non-transactional superclasses,
      you can remove the slot then re-add it.
   c) if the slot is inherited from some transactional superclass,
      you are out of luck: the only way is discarding the whole class
      and recursively applying one of these workarounds on all superclasses."
           
            class-name class-name slot-name option old-value option new-value
            class-name class-name)))



(defun set-assoc (key datum alist)
  "implementation of (SETF ASSOC)"
  (let ((cons (assoc key alist)))
    (if cons
        (setf (rest cons) datum)
        (setf alist (acons key datum alist))))
  alist)

(defun set-assoc* (key datum alist-cons)
  (setf (first alist-cons)
        (set-assoc key datum (first alist-cons)))
  alist-cons)

(defun check-tclass-slot-name (class-name slot-name errors)
  (unless slot-name
    (set-assoc* (gensym)
                (list 'error-tclass-slot-name class-name slot-name)
                errors)))

(defun check-tclass-slot-change (class-name slot-name option old-value new-value errors)
  (unless (equal old-value new-value)
    (set-assoc* slot-name
                (list 'cerror-tclass-slot-change class-name slot-name
                      option old-value new-value)
                errors)))




(defun adjust-transactional-slot-form (class-name class-precedence-list
                                       slot-form old-effective-slot errors)
  "Adjust a single slot definition for a transactional class.
Unless the slot is defined as :transactional NIL,
wrap its :initform with a TVAR and alter its :type to also accept TVARs"
  (declare (type symbol class-name)
           (type list class-precedence-list)
           (type (or symbol list) slot-form)
           (type (or null transactional-effective-slot) old-effective-slot)
           (type cons errors))
            
  (let* ((slot-name (slot-form-name slot-form))
         (is-tx (slot-form-transactional? slot-form))
         (was-tx (if old-effective-slot
                      (transactional-slot? old-effective-slot)
                      is-tx)))

    (check-tclass-slot-name class-name slot-name errors)
    (check-tclass-slot-change class-name slot-name :transactional was-tx is-tx errors)
    
    (unless is-tx
      ;; not transactional, just return
      (return-from adjust-transactional-slot-form slot-form))

    ;; cons a fresh, mutable slot-form
    (setf slot-form (copy-slot-form slot-form))

    (let* ((plist (rest slot-form))
           
           (type? (member :type plist))
           (type (second type?))
           
           (super-tx?   nil) ;; super-slot type is already TVAR-aware?
           (super-type? nil)
           (super-type  nil)

           (super-slots (list-direct-slot-in-superclasses slot-name class-precedence-list)))

      ;; CLOS allows to add type restrictions to a derived slot type,
      ;; but forbids adding alternatives... we cannot add TVARs as allowed :type,
      ;; the only hope is for them to be already allowed
      
      ;; find the most restrictive :type
      (loop for slot in super-slots
         until super-type?
         do
           (unless super-type?
             (setf super-tx?   (transactional-slot? slot)
                   super-type? (slot-definition-type slot)
                   super-type  super-type?)))
      
      (unless (or super-tx? (member super-type '(t nil))
                  (typespec-allows? 'tvar super-type))
        (warn "STMX error!
Transactional class ~S defines transactional slot ~S to override
a non-transactional superclass slot with the same name,
but the superslot already contains the definition :type ~S.
STMX cannot add a type exception to also store TVARs in slot ~S,
expect type errors when instantiating ~S!

Solution: remove the type definition from superslot, or at least explicitly
add TVAR as one of the slot allowed types, as for example:
:type (OR ~S TVAR)
"
              class-name slot-name super-type slot-name class-name super-type))

      (when type?
        ;; if :type is already TVAR-aware, keep things simple and don't repeat TVAR
        (setf (getf plist :type) (if (member type '(t nil)) t `(or ,type tvar))))


      ;; transactional slots do not (yet) support :allocation :class
      (let ((allocation (getf plist :allocation)))
        (loop for slot in super-slots
           until allocation
           do
             (setf allocation (slot-definition-allocation slot)))
        (when (eq :class allocation)
          (error 'stmx.compile-error
                 "transactional slots do not (yet) support :allocation :class
Maybe use :allocation :instance in slot ~S ?" slot-name)))

      (cons slot-name plist))))
    

(defun adjust-transactional-slot-forms (class-name direct-superclasses-names
                                        slot-forms errors)
  "Adjust each slot definition for a transactional class:
unless a slot is defined as :transactional NIL, wrap its :initform with a TVAR
and alter its :type to also accept TVARs"
  (declare (type list direct-superclasses-names slot-forms)
           (type symbol class-name)
           (type cons errors))

  (let* ((class-precedence-list
          (clos-compute-class-precedence-list class-name
                                              direct-superclasses-names))

         ;; search for pre-existing class with same name,
         ;; and collect its slots to check if their :transactional flags changed
         (class (find-class-or-nil class-name))

         (old-direct-slots (when class
                             (ensure-finalized class)
                             (class-direct-slots class)))

         (old-effective-slots (when class (class-slots class))))

    (loop for class in class-precedence-list do
         (ensure-finalized class))

    ;; loop on direct slots discarded from class,
    ;; and check that replacing them with the definition inherited from superclasses
    ;; does not change their transactional flag
    (dolist (old-direct-slot old-direct-slots)

      (let* ((slot-name (slot-definition-name old-direct-slot))
             (old-effective-slot (find-slot-by-name slot-name old-effective-slots)))
        ;; ABCL likes to return dummy slots, filled with NIL
        (when (and slot-name old-effective-slot)
          (let* ((was-tx (transactional-slot? old-effective-slot))
                 (slot-form (find-slot-form-by-name slot-name slot-forms))

                 (is-tx (if slot-form
                            ;; easy, just use the new definition
                            (slot-form-transactional? slot-form)
                            ;; slot is no longer direct...
                            ;; search for it among the superclasses.
                            ;; no need to detect conflicts among the superclasses,
                            ;; COMPUTE-EFFECTIVE-SLOT-DEFINITION will do that
                            (loop for superclass in class-precedence-list
                               thereis
                                 (some #'transactional-slot? (class-direct-slots superclass))))))

            (check-tclass-slot-change class-name slot-name
                                      :transactional was-tx is-tx errors)))))
    
    (loop for slot-form in slot-forms
       for slot-name = (slot-form-name slot-form)
       for old-effective-slot = (find-slot-by-name slot-name old-effective-slots)
       collect (adjust-transactional-slot-form class-name class-precedence-list
                                               slot-form old-effective-slot errors))))
                                               


(defmacro do-class-transactional-effective-slots ((slot class) &body body)
  "Execute BODY on every transactional slot (direct or inherited) of CLASS."
  `(dolist (,slot (class-slots ,class))
     ;; Only loop on those where `transactional-slot?' is true.
     (when (and (typep ,slot 'transactional-effective-slot)
                (transactional-slot? ,slot))
       ,@body)))


(defmacro do-class-transactional-direct-slots ((slot class) &body body)
  "Execute BODY on every transactional direct slot (not inherited) of CLASS."
  `(dolist (,slot (class-direct-slots ,class))
     ;; Only loop on those where `transactional-slot?' is true.
     (when (and (typep ,slot 'transactional-direct-slot)
                (transactional-slot? ,slot))
       ,@body)))



(defun list-class-transactional-direct-slots (class)
  (let ((slots))
    (do-class-transactional-direct-slots (slot class)
      (push slot slots))
    (nreverse slots)))


(defun undefine-method-before (generic-func-name class-name)
  "Remove from GENERIC-FUNC-NAME the method qualified :BEFORE and specialized for CLASS-NAME"
  (let ((class (find-class-or-nil class-name)))
    (when class
      (let* ((generic-func (fdefinition generic-func-name))
             (method (or (find-method generic-func '(:before) `(,class-name) nil)
                         (find-method generic-func '(:before) `(,class) nil))))
        (when method
          (remove-method generic-func method)
          t)))))
  

(defun undefine-method (generic-func-name class-name)
  "Remove from GENERIC-FUNC-NAME the method specialized for CLASS-NAME"
  (let ((class (find-class-or-nil class-name)))
    (when class
      (let* ((generic-func (fdefinition generic-func-name))
             (method (or (find-method generic-func nil `(,class-name) nil)
                         (find-method generic-func nil `(,class) nil))))
        (when method
          (remove-method generic-func method)
          t)))))



;; STMX v2.0.0 optimization:
;; treat initargs and initforms uniformly, i.e. put a TVAR in each transactional slot
;; in INITIALIZE-INSTANCE :before method, and let SLOT-VALUE-USING-CLASS
;; store initargs and initforms into the tvars during normal inizialization.
;;
;; In this way, we no longer need to patch the slots
;; *PARTIALLY* by wrapping initforms with (tvar ,initform) at class declaration time,
;; and *PARTIALLY* by reflectively modifying initargs at runtime
;; inside INITIALIZE-INSTANCE main method
;;
;; This has two benefits:
;; 1) performance: overriding INITIALIZE-INSTANCE main method is slow.
;; 2) uniformity and bug freedom: now a programmer can declare :initform (tvar ...)
;;    and it will work as expected.
;;
;; Problem: this trick breaks CMUCL and ABCL, which bypass SLOT-VALUE-USING-CLASS
;; during CLOS objects initialization. In those cases, define instead the main method
;; INITIALIZE-INSTANCE ((obj class-name))
;; In any case, *never* define any method INITIALIZE-INSTANCE :after ((obj class-name))
;; keep them available for application programmers
(defmacro define-method-initialize-instance (class-name direct-slot-forms)
  (let ((tx-slot-forms
         (loop for slot-form in direct-slot-forms
            when (slot-form-transactional? slot-form)
            collect slot-form)))

    (block nil
      (unless tx-slot-forms
        (return
          `(eval-always
             (undefine-method-before 'initialize-instance ',class-name)
             (undefine-method        'initialize-instance ',class-name))))

      
      #?+use-initialize-instance-before
      (with-gensym obj
        `(progn
           (eval-always
             (undefine-method 'initialize-instance ',class-name))
           
           (defmethod initialize-instance :before ((,obj ,class-name) &key &allow-other-keys)
             ,(format nil "Put a TVAR into every transactional direct slot of ~S
*before* the normal slots initialization." class-name)
             (stmx::without-recording-with-show-tvars
               ,@(loop for slot-form in tx-slot-forms
                    for slot-name = (slot-form-name slot-form)
                    collect `(setf (slot-value ,obj ',slot-name) (tvar)))))))


      #?-use-initialize-instance-before
      (with-gensym obj
        `(progn
           (eval-always
             (undefine-method-before 'initialize-instance ',class-name))

           (defmethod initialize-instance ((,obj ,class-name) &key &allow-other-keys)
             ,(format nil "Put a TVAR into every transactional direct slot of ~S
*after* the normal slots initialization." class-name)

             (stmx::without-recording-with-show-tvars
               (call-next-method)
               ,@(loop for slot-form in tx-slot-forms
                    for slot-name = (slot-form-name slot-form)
                    collect `(tvar-wrap-slot-macro ,obj ',slot-name)))))))))

        


(defmacro transactional-class ((defclass class-name direct-superclasses
                                 direct-slots &rest class-options))
  "Define CLASS-NAME as a new transactional class.
Use this macro to wrap a normal DEFCLASS as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))

The effect is the same as DEFCLASS, plus:
- by default, slots are transactional memory (implemented by TVARs)
- it inherits also from TRANSACTIONAL-OBJECT
- the metaclass is TRANSACTIONAL-CLASS
- it internally defines a method INITIALIZE-INSTANCE :before, do NOT redefine it"
  (let* ((errors (cons nil nil))
         (direct-slots
          (adjust-transactional-slot-forms class-name direct-superclasses direct-slots errors)))
    `(progn
       ,@(mapcar #'rest (first errors))
       (,defclass ,class-name ,(ensure-transactional-object-among-superclasses direct-superclasses)
         ,direct-slots
         ,@class-options
         ,@(stmx.lang::get-feature 'tclass-options)
         (:metaclass transactional-class))
       (define-method-initialize-instance ,class-name ,direct-slots)
       (eval-when (:execute)
         (find-class ',class-name nil)))))



(defmacro transactional ((defclass-or-defstruct class-or-struct-name
                             &rest direct-superclasses-slots-and-options))
  "Define CLASS-OR-STRUCT-NAME as a new transactional class or struct.

Use this macro to wrap a normal DEFCLASS as follows:
\(TRANSACTIONAL (DEFCLASS class-name (superclasses) (slots) [options]))

Or to wrap a normal DEFCLASS as follows:
\(TRANSACTIONAL (DEFSTRUCT (class-name (options)) slots))"
  (ecase defclass-or-defstruct
    (defclass
          `(transactional-class
            (defclass ,class-or-struct-name ,@direct-superclasses-slots-and-options)))
    (defstruct
        `(transactional-struct
           (defstruct ,class-or-struct-name ,@direct-superclasses-slots-and-options)))))
