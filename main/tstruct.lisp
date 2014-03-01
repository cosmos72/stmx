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

(defgeneric tstruct-defs (struct-type)
  (:documentation "Return the list of TSTRUCT-DEF containing the definition
of STRUCT-TYPE"))

(defmethod tstruct-defs (struct-type)
  ;; default implementation
  nil)


(define-global +default+ (gensym "DEFAULT"))

(defstruct tstruct-def
  (name             nil :type symbol)
  (conc-name  +default+ :type t)
  (constructors     nil :type list)
  (copier     +default+ :type symbol)
  (other-options    nil :type list)
  (superclass       nil :type symbol)
  (slots            nil :type list))
                  


(defun names-to-symbol (package &rest names)
  (the symbol
    (intern
     (apply #'concatenate 'string (mapcar #'string names))
     package)))
  

(defun %impl/names-to-symbol (package &rest names)
  (apply #'names-to-symbol package (symbol-name '%stmx-impl/) names))
      

(defun tstruct-def-package (struct-def)
  (declare (type tstruct-def struct-def))
  (symbol-package (tstruct-def-name struct-def)))


(defun tstruct-def-tx-conc-name (struct-def)
  (declare (type tstruct-def struct-def))
  (if-bind conc-name (tstruct-def-conc-name struct-def)
     (%impl/names-to-symbol (tstruct-def-package struct-def) conc-name)
     conc-name))


(defun tstruct-def-tx-constructor (struct-def constructor)
  (declare (type tstruct-def struct-def)
           (type list constructor))
  (if (first constructor)
      (cons (%impl/names-to-symbol (tstruct-def-package struct-def) (first constructor))
            (rest constructor))
      constructor))


(defun tstruct-def-tx-copier (struct-def)
  (declare (type tstruct-def struct-def))
  (if-bind copier (tstruct-def-copier struct-def)
     (%impl/names-to-symbol (tstruct-def-package struct-def) copier)
     copier))


(defun tstruct-def-tx-constructors (struct-def)
  (declare (type tstruct-def struct-def))
  (loop for constructor in (tstruct-def-constructors struct-def)
     collect (tstruct-def-tx-constructor struct-def constructor)))



(defun tstruct-def-all-slots (struct-def)
  "Return all slots of a struct, including inherited ones."
  (declare (type tstruct-def struct-def))

  (let1 slots nil
    (dolist (struct (cons struct-def (tstruct-defs (tstruct-def-superclass struct-def))))
      (dolist (slot (tstruct-def-slots struct))
        (push slot slots)))

    (nreverse slots)))
    


(defun tstruct-def->form (struct-def)
  (declare (type tstruct-def struct-def))

  `(,(tstruct-def-name  struct-def)

     ;; respect order documented by DEFSTRUCT
     (:conc-name   ,(tstruct-def-tx-conc-name    struct-def))

     ,@(loop for tx-constructor in (tstruct-def-tx-constructors struct-def)
          collect `(:constructor ,@tx-constructor))

     (:copier      ,(tstruct-def-tx-copier       struct-def))
     
     ,@(tstruct-def-other-options   struct-def)))


(defun parse-tstruct-def (name-and-options)
  (declare (type (or symbol cons) name-and-options))

  (let* ((symbol? (symbolp name-and-options))
         (name    (if symbol? name-and-options (first name-and-options)))
         (pkg     (symbol-package name))
         (options (if symbol? nil (rest name-and-options)))
         (conc-name     (names-to-symbol pkg name '-))
         (constructors  nil)
         (copier        +default+)
         (superclass    nil)
         (other-options nil))
    
    (dolist (option-and-args options)
      (let* ((option-symbol? (symbolp option-and-args))
             (option         (if option-symbol? option-and-args (first option-and-args)))
             (args           (if option-symbol? +default+   (or (rest  option-and-args) +default+)))
             (arg0           (if (consp args) (first args) args)))
        (case option
          (:conc-name   (setf conc-name arg0))
          (:constructor (push args constructors))
          (:copier      (setf copier arg0))
          (otherwise
           (when (eq :include option)
             (setf superclass arg0))

           (push option-and-args other-options)))))

      (make-tstruct-def
       :name           name
       :conc-name      (if (or (eq conc-name +default+) (eq conc-name nil))
                           (intern "" pkg)
                           conc-name)
       :constructors   (let1 default-constructor (list (names-to-symbol pkg 'make- name))
                         (or
                          (nsubstitute default-constructor +default+
                                       (nreverse constructors))
                          (list default-constructor)))
       :copier         (if (eq copier +default+)
                           (names-to-symbol pkg 'copy- name)
                           copier)
       :other-options  (nreverse other-options)
       :superclass     (unless (eq superclass +default+) superclass))))


(defstruct tstruct-slot
  (name           nil :type symbol)
  (initform       nil :type t)
  (type           t   :type (or symbol cons))
  (read-only      nil :type boolean)
  (transactional  t   :type boolean)
  (other-options  nil :type list))
                  

(defun tstruct-slot->form (slot)
  (declare (type (or string tstruct-slot) slot))

  (if (stringp slot)
      slot
     
      (let ((name           (tstruct-slot-name slot))
            (initform       (tstruct-slot-initform      slot))
            (transactional? (tstruct-slot-transactional slot)))
        `(,name
            ,(if transactional? `(tvar ,initform) initform)
            :type ,(if transactional? 'tvar (tstruct-slot-type slot))
            :read-only ,(tstruct-slot-read-only slot)
            ,@(tstruct-slot-other-options slot)))))
     

(defun tstruct-slot-accessor (slot struct-def)
  "Return the accessor symbol of given struct slot"
  (declare (type tstruct-slot slot)
           (type tstruct-def  struct-def))

  (let* ((struct-name   (tstruct-def-name  struct-def))
         (conc-name     (tstruct-def-conc-name struct-def))
         (pkg           (symbol-package struct-name))
         (slot-name     (tstruct-slot-name slot)))
    (names-to-symbol pkg conc-name slot-name)))


(defun tstruct-slot-tx-accessor (slot struct-def)
  "Return the %stmx-impl accessor symbol of given struct slot"
  (declare (type tstruct-slot slot)
           (type tstruct-def  struct-def))

  (let* ((struct-name   (tstruct-def-name  struct-def))
         (tx-conc-name  (tstruct-def-tx-conc-name struct-def))
         (pkg           (symbol-package struct-name))
         (slot-name     (tstruct-slot-name slot))
         (tx-accessor-name (names-to-symbol pkg tx-conc-name slot-name)))
    tx-accessor-name))
           


(defun parse-tstruct-slot (slot)
  (declare (type (or string symbol cons) slot))

  ;; documentation?
  (when (stringp slot)
    (return-from parse-tstruct-slot slot))
   
  (when (symbolp slot)
    (setf slot (list slot nil)))

  (let1 read-only (getf slot :read-only)
    (check-type read-only boolean)

    (make-tstruct-slot
     :name       (first  slot)
     :initform   (second slot)
     :type       (getf slot :type t)
     :read-only  read-only
     ;; transactional-struct direct slots are transactional by default.
     ;; to make them non-transactional, add the option :transactional nil
     ;;
     ;; exception: read-only slots are never transactional
     :transactional (let1 transactional (getf slot :transactional +default+)
                      (unless (member transactional `(t nil ,+default+))
                        (error "STMX: unexpected slot option :TRANSACTIONAL ~S,
expecting :TRANSACTIONAL T or :TRANSACTIONAL NIL" transactional))
                      
                      (when (and read-only
                                 (eq t transactional))
                        (error "STMX: incompatible slot options :READ-ONLY ~S and :TRANSACTIONAL ~S,
read-only slots cannot be transactional" read-only transactional))

                      (when (eq transactional +default+)
                        (setf transactional (not read-only)))

                      transactional)

     :other-options (let1 other-options (copy-list (rest (rest slot)))
                      (dolist (option '(:type :read-only :transactional))
                        (remf other-options option))
                      other-options))))
                       
   
(defun parse-tstruct-slots (slots)
  (declare (type list slots))
  (loop for slot in slots
     collect (parse-tstruct-slot slot)))


(defun make-load-form/tstruct-slot (obj)
  (declare (type tstruct-slot obj))

  `(make-tstruct-slot
    :name       ',(tstruct-slot-name obj)
    :initform   ',(tstruct-slot-initform obj)
    :type       ',(tstruct-slot-type obj)
    :read-only  ',(tstruct-slot-read-only obj)
    :transactional ',(tstruct-slot-transactional obj)
    :other-options ',(tstruct-slot-other-options obj)))

                       
(defun make-load-form/tstruct-def (obj)
  (declare (type tstruct-def obj))

  `(make-tstruct-def
    :name     ',(tstruct-def-name obj)
    :copier   ',(tstruct-def-copier obj)
    :conc-name ',(tstruct-def-conc-name obj)
    :constructors ',(tstruct-def-constructors obj)
    :other-options ',(tstruct-def-other-options obj)
    :superclass    ',(tstruct-def-superclass obj)
    :slots           (list ,@(loop for slot in (tstruct-def-slots obj)
                                collect  (make-load-form/tstruct-slot slot)))))


(defun tstruct-def->defun-constructor (struct-def constructor-name-and-arglist)
  "Define a single constructor function"
  (declare (type tstruct-def struct-def)
           (type list constructor-name-and-arglist))

  (when (rest constructor-name-and-arglist)
    (error "CONSTRUCTOR-ARGLIST option not yet implemented
in (TRANSACTIONAL-STRUCT (DEFSTRUCT (~A (:CONSTRUCTOR ...))))" (tstruct-def-name struct-def)))


  (let ((slots (tstruct-def-all-slots struct-def))
        (constructor-name (first constructor-name-and-arglist)))

    ;; (:constructor nil) means "suppress the constructor"
    (when constructor-name
      `(defun ,constructor-name (&key ,@(loop for slot in slots
                                           collect (list (tstruct-slot-name slot)
                                                         (tstruct-slot-initform slot))))
         (declare ,@(loop for slot in slots
                       collect `(type ,(tstruct-slot-type slot) ,(tstruct-slot-name slot))))
         
         (,(first (tstruct-def-tx-constructor struct-def constructor-name-and-arglist))
           ,@(loop for slot in slots
                collect (intern (symbol-name (tstruct-slot-name slot)) :keyword)

                collect (let1 arg-name (tstruct-slot-name slot)
                          (if (tstruct-slot-transactional slot)
                              `(tvar ,arg-name)
                              arg-name))))))))



(defun tstruct-def->defun-constructors (struct-def)
  "Define constructor functions"
  (declare (type tstruct-def struct-def))
  (loop for constructor in (tstruct-def-constructors struct-def)
     collect (tstruct-def->defun-constructor struct-def constructor)))


(defun tstruct-def->defun-accessors (struct-def)
  "Define accessor functions"
  (declare (type tstruct-def struct-def))

  (let ((defuns nil)
        (name (tstruct-def-name struct-def)))
    
    (with-gensyms (instance value)
      (dolist (struct (cons struct-def (tstruct-defs (tstruct-def-superclass struct-def))))
        (dolist (slot (tstruct-def-slots struct))

          (let ((accessor    (tstruct-slot-accessor slot struct))
                (tx-accessor (tstruct-slot-tx-accessor slot struct))
                (transactional (tstruct-slot-transactional slot))
                (type        (tstruct-slot-type slot)))
            
            (unless transactional
              (push `(declaim (inline ,accessor))
                    defuns))

            (push `(defun ,accessor (,instance)
                     (declare (type ,name ,instance))
                     (the (values ,type &optional)
                       ,(if transactional
                            `($ (,tx-accessor ,instance))
                            `(,tx-accessor ,instance))))
                  defuns)

            (unless (tstruct-slot-read-only slot)

              (unless transactional
                (push `(declaim (inline (setf ,accessor)))
                      defuns))

              (push `(defun (setf ,accessor) (,value ,instance)
                       (declare (type ,name ,instance)
                                (type ,type ,value))
                       (the (values ,type &optional)
                         ,(if transactional
                              `(setf ($ (,tx-accessor ,instance)) ,value)
                              `(setf (,tx-accessor ,instance) ,value))))
                    defuns))))))
    (nreverse defuns)))



(defun tstruct-def->defun-copier (struct-def)
  "Define copier function"
  (declare (type tstruct-def struct-def))
           
  (when-bind copier (tstruct-def-copier struct-def)
    (let ((name      (tstruct-def-name    struct-def))
          (tx-copier (tstruct-def-tx-copier struct-def)))
      (declare (ignorable name))

      (with-gensyms (old-instance new-instance)
        `(defun ,copier (,old-instance)
           (declare (type ,name ,old-instance))
           (let ((,new-instance (,tx-copier ,old-instance)))

             ,@(let1 setf-args nil
                 (dolist (struct (cons struct-def (tstruct-defs
                                                   (tstruct-def-superclass struct-def))))
                   (dolist (slot (tstruct-def-slots struct))
                     (when (tstruct-slot-transactional slot)
                       (let1 tx-accessor (tstruct-slot-tx-accessor slot struct)
                         ;; we will reverse setf-args later
                         (push `(,tx-accessor ,new-instance) setf-args)
                         (push `(tvar ($ (,tx-accessor ,old-instance))) setf-args)))))
                 (cond
                   ((third setf-args) `((atomic (setf ,@(nreverse setf-args)))
                                        ,new-instance))
                   ((second setf-args) `((setf ,@(nreverse setf-args))
                                         ,new-instance))
                   (t                  `(,new-instance))))))))))


           
  
(defun tstruct-def->functions (struct-def slots)
  "Define constructors, copier and accessors functions"
  (declare (type tstruct-def struct-def)
           (type list slots))

  (let ((name   (tstruct-def-name   struct-def)))

    (setf (tstruct-def-slots struct-def) (remove-if #'stringp slots))
    
    (with-gensym struct-defs-var
      (delete
       nil
       `((eval-always
           (let ((,struct-defs-var (cons ,(make-load-form/tstruct-def struct-def)
                                         (tstruct-defs
                                          ',(tstruct-def-superclass struct-def)))))
             (defmethod tstruct-defs ((type (eql ',name)))
               ,struct-defs-var)))
           
         ,@(tstruct-def->defun-constructors struct-def)
         
         ,(tstruct-def->defun-copier struct-def)
         
         ,@(tstruct-def->defun-accessors struct-def))))))
     


             


(defmacro transactional-struct ((defstruct name-and-options &rest slot-descriptions))
  "Define NAME as a new transactional struct.
Use this macro to wrap a normal DEFSTRUCT as follows:
\(TRANSACTIONAL-STRUCT (DEFSTRUCT {struct-name | (struct-name [options])} slots-description*)

The effect is the same as DEFSTRUCT, plus:
- by default, direct slots are transactional memory (implemented by TVARs)"

  (let ((struct-def (parse-tstruct-def name-and-options))
        (slots (parse-tstruct-slots slot-descriptions)))

    `(progn
       (,defstruct ,(tstruct-def->form struct-def)
         ,@(mapcar #'tstruct-slot->form slots))

       ,@(tstruct-def->functions struct-def slots))))


