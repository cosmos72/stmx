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

;;;; * Transactional structures

(defgeneric transactional-struct-defs (struct-type)
  (:documentation "Return the list of TRANSACTIONAL-STRUCT-DEF containing the definition
of STRUCT-TYPE"))

(defmethod transactional-struct-defs (struct-type)
  ;; default implementation
  nil)


(define-global +default+ (gensym "DEFAULT"))

(defstruct transactional-struct-def
  (name             nil :type symbol)
  (conc-name  +default+ :type t)
  (constructors     nil :type list)
  (copier     +default+ :type symbol)
  (other-options    nil :type list)
  (superclass       nil :type symbol)
  (tx-conc-name     nil :type symbol)
  (tx-constructor   nil :type symbol)
  (tx-copier        nil :type symbol)
  (tx-slots         nil :type list))
                  

(defun transactional-struct-def->form (struct-def)
  (declare (type transactional-struct-def struct-def))

  (let ((name           (transactional-struct-def-name           struct-def))
        (tx-conc-name   (transactional-struct-def-tx-conc-name   struct-def))
        (tx-constructor (transactional-struct-def-tx-constructor struct-def))
        (tx-copier      (transactional-struct-def-tx-copier      struct-def))
        (other-options  (transactional-struct-def-other-options  struct-def)))
    `(,name
      ;; respect order documented by DEFSTRUCT
      (:conc-name   ,tx-conc-name)
      (:constructor ,tx-constructor)
      (:copier      ,tx-copier)
      ,@other-options)))


(defun names-to-symbol (package &rest names)
  (the symbol
    (intern
     (apply #'concatenate 'string (mapcar #'string names))
     package)))
  

(defun parse-transactional-struct-def (name-and-options)
  (declare (type (or symbol cons) name-and-options))

  (let* ((symbol? (symbolp name-and-options))
         (name    (if symbol? name-and-options (first name-and-options)))
         (pkg     (symbol-package name))
         (options (if symbol? nil (rest name-and-options)))
         (conc-name     (names-to-symbol pkg name '-))
         (constructors  nil)
         (copier        (names-to-symbol pkg 'copy- name))
         (superclass    nil)
         (other-options nil))
    
    (dolist (option-and-args options)
      (let* ((option-symbol? (symbolp option-and-args))
             (option         (if option-symbol? option-and-args (first option-and-args)))
             (args           (if option-symbol? +default+       (rest  option-and-args)))
             (arg0           (if (consp args) (first args) args)))
        (case option
          (:conc-name   (setf conc-name arg0))
          (:constructor (push args constructors))
          (:copier      (setf copier arg0))
          (otherwise
           (when (eq :include option)
             (setf superclass arg0))

           (push option-and-args other-options)))))

    (flet ((%impl/names-to-symbol (&rest names)
             (the (values symbol &optional)
               (apply #'names-to-symbol pkg (symbol-name '%stmx-impl/) names))))
      
      (make-transactional-struct-def
       :name           name
       :copier         (if (eq copier +default+)
                           (names-to-symbol pkg 'copy- name) ;; defauly copier
                           copier)
       :conc-name      (if (or (eq conc-name nil)
                               (eq conc-name +default+))
                           (names-to-symbol pkg name '-) ;; default conc-name
                           conc-name)
       :constructors   (let1 default-constructor (names-to-symbol pkg 'make- name)
                         (or
                          (nsubstitute default-constructor +default+
                                       (nreverse constructors))
                          (list default-constructor)))
       :other-options  (nreverse other-options)
       :superclass     (unless (eq superclass +default+)
                         superclass)
       :tx-conc-name   (%impl/names-to-symbol name "-")
       :tx-constructor (%impl/names-to-symbol 'make- name)
       :tx-copier      (when copier (%impl/names-to-symbol 'copy- name))))))


(defstruct transactional-struct-slot
  (name           nil :type symbol)
  (initform       nil :type t)
  (type           t   :type (or symbol cons))
  (read-only      nil :type boolean)
  (transactional  t   :type boolean)
  (other-options  nil :type list))
                  

(defun transactional-struct-slot->form (slot-obj)
  (declare (type (or string transactional-struct-slot) slot-obj))

  (let ((name           (transactional-struct-slot-name slot-obj))
        (initform       (transactional-struct-slot-initform      slot-obj))
        (transactional? (transactional-struct-slot-transactional slot-obj)))
    `(,name
      ,(if transactional? `(tvar ,initform) initform)
      :type ,(if transactional? 'tvar (transactional-struct-slot-type slot-obj))
      :read-only ,(transactional-struct-slot-read-only slot-obj)
      ,@(transactional-struct-slot-other-options slot-obj))))
     

(defun transactional-struct-slot-accessor (slot struct-def)
  "Return the accessor symbol of given struct slot"
  (declare (type transactional-struct-slot slot)
           (type transactional-struct-def  struct-def))

  (let* ((struct-name   (transactional-struct-def-name  struct-def))
         (conc-name     (transactional-struct-def-conc-name struct-def))
         (pkg           (symbol-package struct-name))
         (slot-name     (transactional-struct-slot-name slot)))
    (names-to-symbol pkg conc-name slot-name)))


(defun transactional-struct-slot-tx-accessor (slot struct-def)
  "Return the %stmx-impl accessor symbol of given struct slot"
  (declare (type transactional-struct-slot slot)
           (type transactional-struct-def  struct-def))

  (let* ((struct-name   (transactional-struct-def-name  struct-def))
         (tx-conc-name  (transactional-struct-def-tx-conc-name struct-def))
         (pkg           (symbol-package struct-name))
         (slot-name     (transactional-struct-slot-name slot))
         (tx-accessor-name (names-to-symbol pkg tx-conc-name slot-name)))
    (break)
    tx-accessor-name))
           


(defun parse-transactional-struct-slot (slot)
  (declare (type (or string symbol cons) slot))

  ;; documentation?
  (when (stringp slot)
    (return-from parse-transactional-struct-slot slot))
   
  (when (symbolp slot)
    (setf slot (list slot nil)))

  (make-transactional-struct-slot
   :name       (first  slot)
   :initform   (second slot)
   :type       (getf slot :type t)
   :read-only  (getf slot :read-only)
   ;; transactional-struct direct slots are transactional by default.
   ;; to make them non-transactional, add the option :transactional nil
   :transactional (getf slot :transactional t)
   :other-options (let1 other-options (copy-list (rest (rest slot)))
                    (dolist (option '(:type :read-only :transactional))
                      (remf other-options option))
                    other-options)))
                       

   

(defun parse-transactional-struct-slots (slots)
  (declare (type list slots))
  (loop for slot in slots
     collect (parse-transactional-struct-slot slot)))


(defun transactional-struct-def->defun-copier (struct-def)
  "Define copier function"
  (declare (type transactional-struct-def struct-def))
           
  (when-bind copier (transactional-struct-def-copier struct-def)
    (let ((name      (transactional-struct-def-name    struct-def))
          (tx-copier (transactional-struct-def-tx-copier struct-def)))
      (with-gensyms (old-instance new-instance)
        `(defun ,copier (,old-instance)
           (declare (type ,name ,old-instance))
           (let ((,new-instance (,tx-copier ,old-instance)))

             ,@(let1 setf-args nil
                  (loop for struct = struct-def then (transactional-struct-def-superclass struct)
                     while struct do
                       (loop for slot in (transactional-struct-def-tx-slots struct)
                          when (transactional-struct-slot-transactional slot)
                          do
                            (let1 tx-accessor (transactional-struct-slot-tx-accessor slot struct)
                              ;; we will reverse setf-args later
                              (push `(,tx-accessor ,new-instance) setf-args)
                              (push `(tvar ($ (,tx-accessor ,old-instance))) setf-args))))
                  (if setf-args
                      `((setf ,@(nreverse setf-args))
                        ,new-instance)
                      `(,new-instance)))))))))


(defun transactional-struct-def->functions (struct-def slot-objs)
  "Define constructors, copier and accessors functions"
  (declare (type transactional-struct-def struct-def)
           (type list slot-objs))

  (let* ((name   (transactional-struct-def-name   struct-def))
         ;;(pkg    (symbol-package name))
         (accessors (loop for slot in slot-objs
                       unless (stringp slot)
                       when (transactional-struct-slot-transactional slot)
                       collect
                         (cons (transactional-struct-slot-accessor slot struct-def)
                               (transactional-struct-slot-tx-accessor slot struct-def)))))

    (setf (transactional-struct-def-tx-slots struct-def) (remove-if #'stringp slot-objs))

    (delete
     nil
     `((eval-always
         (defmethod transactional-struct-defs ((type (eql ',name)))
           (cons ,struct-def (call-next-method))))

       ,(transactional-struct-def->defun-copier struct-def)
       
       ,@(with-gensyms (instance value)
            (loop for (accessor . tx-accessor) in accessors
               collect
                 `(defun ,accessor (,instance)
                    (declare (type ,name ,instance))
                    ($ (,tx-accessor ,instance)))
               collect
                 `(defun (setf ,accessor) (,value ,instance)
                    (declare (type ,name ,instance))
                    (setf ($ (,tx-accessor ,instance)) ,value))))))))

             


(defmacro transactional-struct ((defstruct name-and-options &rest slot-descriptions))
  "Define NAME as a new transactional struct.
Use this macro to wrap a normal DEFSTRUCT as follows:
\(TRANSACTIONAL-STRUCT (DEFSTRUCT {struct-name | (struct-name [options])} slots-description*)

The effect is the same as DEFSTRUCT, plus:
- by default, direct slots are transactional memory (implemented by TVARs)"

  (let ((struct-def (parse-transactional-struct-def name-and-options))
        (slot-objs (parse-transactional-struct-slots slot-descriptions)))

    `(progn
       (,defstruct ,(transactional-struct-def->form struct-def)
         ,@(loop for slot-obj in slot-objs
              collect (transactional-struct-slot->form slot-obj)))

       ,@(transactional-struct-def->functions struct-def slot-objs))))


