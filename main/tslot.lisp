;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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

;;;; * Transactional slots

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
  ;; Get the slot value - it may be a TVAR.
  (let1 obj (call-next-method)

    (if (and (transactional-slot? slot)
             (or (recording?) (hide-tvars?)))

        ;; It is a TVAR, return its value.
        ;; During transactions, reading from tvars is recorded to the current tlog.
        (let1 value ($ (the tvar obj))
          (if (eq value +unbound-tvar+)
              (unbound-slot-error class instance slot)
              value))
    
        ;; Return the plain slot-value.
        obj)))


(defmacro slot-raw-tvar (class instance slot)
  "Return the raw tvar stored in a transactional slot."
  `(without-recording-with-show-tvars
     (slot-value-using-class ,class ,instance ,slot)))

(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))

  (if (and (transactional-slot? slot)
           (or (recording?) (hide-tvars?)))
    
      ;; Get the tvar from the slot and write inside it.
      ;; During transactions, writing tvars is recorded into the current tlog.
      (let1 var (slot-raw-tvar class instance slot)
        (setf ($ var) value))
      
      ;; Write in the slot
      (call-next-method)))



(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))

  (if (and (transactional-slot? slot)
           (or (recording?) (hide-tvars?)))

     ;; Get the tvar from the slot, and return true if it is bound to a value.
     ;; During transactions, the checking whether the tvar is bound
     ;; is performed against the current tlog.
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




;;;; ** Transactional objects reinitialization

(defmethod update-instance-for-redefined-class

    #?+use-initialize-instance-before :before
    
    ((instance transactional-object) added-slots discarded-slots
     property-list #-(and) &rest #-(and) initargs &key &allow-other-keys)
  
  "Put TVARs into all newly-added transactional slots of INSTANCE"

  (declare (ignore discarded-slots property-list))
  
  (stmx::without-recording-with-show-tvars

    #?-use-initialize-instance-before
    (call-next-method)

    (when added-slots
      (let ((effective-slots (class-slots (class-of instance))))
        (dolist (slot-name added-slots)
          (declare (type symbol slot-name))
          (let ((slot (find-slot-by-name slot-name effective-slots)))
            (when (transactional-slot? slot)

              #?+use-initialize-instance-before 
              ;; added a transactional slot. put a TVAR into it
              ;; before its contents is initialized
              (setf (slot-value instance slot-name) (tvar))

              #?-use-initialize-instance-before 
              ;; the new transactional slot is already initialized.
              ;; wrap the slot value with a TVAR.
              (tvar-wrap-slot instance slot-name))))))))
              






;; live migration from STMX v1.9.0: we must remove
;; the methods INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE
;; specialized for TRANSACTIONAL-OBJECT
(eval-when (:execute)
  (undefine-method 'initialize-instance   'transactional-object)
  (undefine-method 'reinitialize-instance 'transactional-object))
