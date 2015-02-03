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
  (conc-name  +default+ :type symbol)
  (constructor      nil :type symbol)
  (copier     +default+ :type symbol)
  (type             nil :type (or symbol list))
  (superclass       nil :type symbol)
  (options          nil :type list)
  (slots            nil :type list))
                  

(defun symbol-package-or-fail (symbol)
  (let ((pkg (symbol-package symbol)))
    (unless pkg
      (error "transactional struct ~S is an uninterned symbol, i.e. its package is NIL.
Cannot generate functions in package NIL" symbol))
    pkg))


(defun names-to-symbol (package &rest names)
  (the symbol
    (intern
     (apply #'concatenate 'string (mapcar #'string names))
     package)))
  

(defun %impl/names-to-symbol (package &rest names)
  (apply #'names-to-symbol package (symbol-name '%stmx-impl/) names))
      

(defun tstruct-def-package (struct-def)
  (declare (type tstruct-def struct-def))
  (symbol-package-or-fail (tstruct-def-name struct-def)))


(defun tstruct-def-tx-conc-name (struct-def)
  (declare (type tstruct-def struct-def))
  (if-bind conc-name (tstruct-def-conc-name struct-def)
     (%impl/names-to-symbol (tstruct-def-package struct-def) conc-name)
     conc-name))


(defun tstruct-def-tx-constructor (struct-def)
  "Return the %stmx-impl constructor name of a tstruct"
  (declare (type tstruct-def struct-def))
  (if-bind constructor (tstruct-def-constructor struct-def)
    (%impl/names-to-symbol (tstruct-def-package struct-def) constructor)
    constructor))



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
     (:conc-name   ,(tstruct-def-tx-conc-name   struct-def))
     (:constructor ,(tstruct-def-tx-constructor struct-def))
     (:copier      nil)
     
     ,@(tstruct-def-options   struct-def)))


(defun parse-tstruct-def (name-and-options)
  (declare (type (or symbol cons) name-and-options))

  (let* ((symbol? (symbolp name-and-options))
         (name    (if symbol? name-and-options (first name-and-options)))
         (pkg     (symbol-package-or-fail name))
         (options (if symbol? nil (rest name-and-options)))
         (conc-name     +default+)
         (constructor   +default+)
         (copier        +default+)
         (type          nil)
         (superclass    nil)
         (other-options nil)
         (seen          nil))

    (labels ((to-string (x)
               (format nil "~S" x))

             (check-unique (option)
               (when (member option seen :test 'eq)
                 (error "error in (~S ~S ...)
~S supports only one option ~S"
                        'defstruct name-and-options 'transactional-struct option)))
           
             (check-unique-01-args (opt option args)
               (check-unique option)
               (when (and (consp args) (rest args))
                 (error "error in (~S ~S ...)
~S expects at most one argument in option ~S
found instead ~A"
                        'defstruct name-and-options 'transactional-struct option
                        (to-string opt))))

             (check-unique-1-arg   (opt option args)
               (check-unique option)
               (unless (and (consp args) (null (rest args)))
                 (error "error in (~S ~S ...)
~S expects exactly one argument in option ~S
found instead ~A"
                        'defstruct name-and-options 'transactional-struct option
                        (to-string opt))))

             (check-unsupported    (opt option args)
               (declare (ignorable opt option args))
               #-(and)
               (case option
                 (()
                  (error "error in (~S ~S ...)
~S does not support option ~A"
                         'defstruct name-and-options 'transactional-struct
                         (to-string opt))))))
      
      (dolist (opt options)
        (let* ((symbol? (symbolp opt))
               (option  (if symbol? opt        (first opt)))
               (args    (if symbol? +default+  (or (rest opt) +default+)))
               (arg0    (if (consp args) (first args) args)))

          (case option
            (:conc-name   (check-unique-01-args opt option args)
                          (setf conc-name arg0))
            (:constructor (check-unique-01-args opt option args)
                          (setf constructor arg0))
            (:copier      (check-unique-01-args opt option args)
                          (setf copier arg0))
            (:include     (check-unique-1-arg   opt option args)
                          (setf superclass arg0)
                          (push opt other-options))
            (:type        (check-unique-1-arg   opt option args)
                          (setf type arg0)
                          (push opt other-options))
            (otherwise    (check-unsupported    opt option args)
                          (push opt other-options)))
          (push option seen))))

    (when (not constructor)
      ;; no constructor? then no copier.
      (if (and copier (member :copier seen))
          (error "error in (~S ~S ...)
~S does not support ~S together with (~S ~S)
reason: the constructor is needed to implement the copier"
                 'defstruct name-and-options 'transactional-struct
                 (if (eq copier +default+) :copier (list :copier copier))
                 :constructor nil)

          (setf copier nil)))
    
    (make-tstruct-def
     :name        name
     :conc-name   (if (eq conc-name +default+)
                      (names-to-symbol pkg name '-)
                      conc-name)
     :constructor (if (eq constructor +default+)
                      (names-to-symbol pkg 'make- name)
                      constructor)
     :copier      (if (eq copier +default+)
                      (names-to-symbol pkg 'copy- name)
                      copier)
     :type        type
     :superclass  superclass
     :options     (nreverse other-options))))


(defstruct tstruct-slot
  (name          nil :type symbol)
  (initform      nil :type t)
  (type          t   :type (or symbol cons))
  (read-only     nil :type boolean)
  (transactional t   :type boolean)
  (options       nil :type list))
                  

(defun tstruct-slot-initialize (struct-name slot-name initform)
  (error "TRANSACTIONAL-STRUCT internal error! ~S constructor did not initialize slot ~S.
Slot initform is ~S"
         struct-name slot-name initform))


(defun tstruct-slot->form (slot struct-def)
  (declare (type (or string tstruct-slot) slot)
           (type tstruct-def struct-def))

  (if (stringp slot)
      slot
     
      (let ((struct-name    (tstruct-def-name struct-def))
            (name           (tstruct-slot-name slot))
            (initform       (tstruct-slot-initform slot))
            (transactional? (tstruct-slot-transactional slot)))
        `(,name
          ;; all tslots are initialized explicitly by constructor
          (tstruct-slot-initialize ',struct-name ',name ',initform)
          :type ,(if transactional? 'tvar (tstruct-slot-type slot))
          :read-only ,(tstruct-slot-read-only slot)
          ,@(tstruct-slot-options slot)))))
     


(defun tstruct-slot-keyword (slot struct-def)
  "Return the constructor keyword argument of given struct slot"
  (declare (type tstruct-slot slot)
           (type tstruct-def  struct-def)
           (ignore struct-def))

  (intern (symbol-name (tstruct-slot-name slot)) :keyword))


(defun tstruct-slot-accessor (slot struct-def)
  "Return the accessor symbol of given struct slot"
  (declare (type tstruct-slot slot)
           (type tstruct-def  struct-def))

  (let* ((struct-name   (tstruct-def-name  struct-def))
         (conc-name     (tstruct-def-conc-name struct-def))
         (pkg           (symbol-package-or-fail struct-name))
         (slot-name     (tstruct-slot-name slot)))
    (names-to-symbol pkg conc-name slot-name)))


(defun tstruct-slot-tx-accessor (slot struct-def)
  "Return the %stmx-impl accessor symbol of given struct slot"
  (declare (type tstruct-slot slot)
           (type tstruct-def  struct-def))

  (let* ((struct-name   (tstruct-def-name  struct-def))
         (tx-conc-name  (tstruct-def-tx-conc-name struct-def))
         (pkg           (symbol-package-or-fail struct-name))
         (slot-name     (tstruct-slot-name slot))
         (tx-accessor-name (names-to-symbol pkg tx-conc-name slot-name)))
    tx-accessor-name))
           


(defun parse-tstruct-slot (slot)
  (declare (type (or string symbol cons) slot))

  ;; documentation?
  (when (stringp slot)
    (return-from parse-tstruct-slot slot))
   
  (cond
    ;; replace slot-name with (slot-name nil)
    ((symbolp slot)     (setf slot (list slot nil)))
    ;; replace (slot-name) with (slot-name nil)
    ((null (rest slot)) (setf slot (list (first slot) nil))))
  

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
                      (unless (member transactional `(t nil ,+default+) :test 'eq)
                        (error "STMX: unexpected slot option :TRANSACTIONAL ~S,
expecting :TRANSACTIONAL T or :TRANSACTIONAL NIL" transactional))
                      
                      (when (and read-only
                                 (eq t transactional))
                        (error "STMX: incompatible slot options :READ-ONLY ~S and :TRANSACTIONAL ~S,
read-only slots cannot be transactional" read-only transactional))

                      (when (eq transactional +default+)
                        (setf transactional (not read-only)))

                      transactional)

     :options (let1 other-options (copy-list (rest (rest slot)))
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
    :name          ',(tstruct-slot-name          obj)
    :initform      ',(tstruct-slot-initform      obj)
    :type          ',(tstruct-slot-type          obj)
    :read-only     ',(tstruct-slot-read-only     obj)
    :transactional ',(tstruct-slot-transactional obj)
    :options       ',(tstruct-slot-options       obj)))

                       
(defun make-load-form/tstruct-def (obj)
  (declare (type tstruct-def obj))

  `(make-tstruct-def
    :name        ',(tstruct-def-name        obj)
    :conc-name   ',(tstruct-def-conc-name   obj)
    :constructor ',(tstruct-def-constructor obj)
    :copier      ',(tstruct-def-copier      obj)
    :type        ',(tstruct-def-type        obj)
    :superclass  ',(tstruct-def-superclass  obj)
    :options     ',(tstruct-def-options     obj)
    :slots       (list ,@(loop for slot in (tstruct-def-slots obj)
                            collect  (make-load-form/tstruct-slot slot)))))


(defun tstruct-def->defun-constructor (struct-def)
  "Define the constructor function.
Note: TRANSACTIONAL-STRUCT supports only one constructor"
  (declare (type tstruct-def struct-def))

  (let ((slots (tstruct-def-all-slots struct-def))
        (constructor-name (tstruct-def-constructor struct-def)))

    ;; (:constructor nil) means "suppress the constructor"
    (when constructor-name
      `(defun ,constructor-name (&key ,@(loop for slot in slots
                                           collect (list (tstruct-slot-name slot)
                                                         (tstruct-slot-initform slot))))
         (declare ,@(loop for slot in slots
                       collect `(type ,(tstruct-slot-type slot) ,(tstruct-slot-name slot))))
         
         (,(tstruct-def-tx-constructor struct-def)
           ,@(loop for slot in slots
                collect (intern (symbol-name (tstruct-slot-name slot)) :keyword)
                  
                collect (let1 arg-name (tstruct-slot-name slot)
                          (if (tstruct-slot-transactional slot)
                              `(tvar ,arg-name)
                              arg-name))))))))


(defun tstruct-def->defun-accessors (struct-def)
  "Define accessor functions"
  (declare (type tstruct-def struct-def))

  (let ((defuns nil)
        (name (tstruct-def-name struct-def))
        (type (tstruct-def-type struct-def)))
    
    (with-gensyms (instance value)
      (dolist (struct (cons struct-def (tstruct-defs (tstruct-def-superclass struct-def))))
        (dolist (slot (tstruct-def-slots struct))

          (let ((accessor      (tstruct-slot-accessor slot struct))
                (tx-accessor   (tstruct-slot-tx-accessor slot struct))
                (transactional (tstruct-slot-transactional slot))
                (slot-type     (tstruct-slot-type slot)))
            
            (unless transactional
              (push `(declaim (inline ,accessor))
                    defuns))

            (push `(defun ,accessor (,instance)
                     (declare (type ,(or type name) ,instance))
                     (the (values ,slot-type &optional)
                       ,(if transactional
                            `($ (,tx-accessor ,instance))
                            `(,tx-accessor ,instance))))
                  defuns)

            (unless (tstruct-slot-read-only slot)

              (unless transactional
                (push `(declaim (inline (setf ,accessor)))
                      defuns))

              (push `(defun (setf ,accessor) (,value ,instance)
                       (declare (type ,(or type name) ,instance)
                                (type ,slot-type ,value))
                       (the (values ,slot-type &optional)
                         ,(if transactional
                              `(setf ($ (,tx-accessor ,instance)) ,value)
                              `(setf (,tx-accessor ,instance) ,value))))
                    defuns))))))
    (nreverse defuns)))


(defun tstruct-def->tx-accessors (struct-def)
  "List %stmx-impl hidden accessor functions,
in order to declaim them inline"
  (declare (type tstruct-def struct-def))

  #+cmucl
  ;; CMUCL chokes if struct accessors are declaimed inline
  (declare (ignore struct-def))

  #-cmucl
  (let ((accessors nil))

    (dolist (struct (cons struct-def (tstruct-defs (tstruct-def-superclass struct-def))))
      (dolist (slot (tstruct-def-slots struct))

        (let ((tx-accessor (tstruct-slot-tx-accessor slot struct)))

          (push tx-accessor accessors)
          (unless (tstruct-slot-read-only slot)
            (push `(setf ,tx-accessor) accessors)))))
    (nreverse accessors)))



(defun tstruct-def->defun-copier (struct-def)
  "Define copier function"
  (declare (type tstruct-def struct-def))
           
  (when-bind copier    (tstruct-def-copier      struct-def)
    (let ((name        (tstruct-def-name        struct-def))
          (type        (tstruct-def-type        struct-def))
          (constructor (tstruct-def-constructor struct-def)))
      
      (with-gensym old-instance
        `(defun ,copier (,old-instance)
           (declare (type ,(or type name) ,old-instance))

           (,constructor
            ,@(let ((args nil))
                (dolist (struct (cons struct-def (tstruct-defs
                                                  (tstruct-def-superclass struct-def))))
                  (dolist (slot (tstruct-def-slots struct))
                    (let ((accessor    (tstruct-slot-accessor slot struct))
                          (arg-keyword (tstruct-slot-keyword slot struct)))
                      ;; we will reverse args later
                      (push arg-keyword args)
                      (push `(,accessor ,old-instance) args))))
                (nreverse args))))))))
         



(defun tstruct-def->defun-functions (struct-def)
  "Define constructors, copier and accessors functions"
  (declare (type tstruct-def struct-def))

  (let ((name   (tstruct-def-name   struct-def)))

    (with-gensym tstruct-defs-var
      (delete
       nil
       `((eval-always
           (let ((,tstruct-defs-var (cons ,(make-load-form/tstruct-def struct-def)
                                          (tstruct-defs
                                           ',(tstruct-def-superclass struct-def)))))
             (defmethod tstruct-defs ((type (eql ',name)))
               ,tstruct-defs-var)))
           
         ,(tstruct-def->defun-constructor struct-def)
         
         ,(tstruct-def->defun-copier struct-def)
         
         ,@(tstruct-def->defun-accessors struct-def))))))
     




(defun tstruct-def->tx-functions (struct-def)
  "List %stmx-impl hidden constructor and accessors functions,
in order to declaim them inline"
  (declare (type tstruct-def struct-def))
  
  (delete
   nil
   `(,(tstruct-def-tx-constructor struct-def)
       
     ,@(tstruct-def->tx-accessors  struct-def))))
     


(defun tstruct-def->all (defstruct struct-def)
  "Return all the forms needed to define a transactional struct"
  (declare (type tstruct-def struct-def))

  (let ((slots (tstruct-def-slots struct-def)))
    `(progn
       (declaim (inline ,@(tstruct-def->tx-functions struct-def)))
       
       (,defstruct ,(tstruct-def->form struct-def)
         ,@(loop for slot in slots
                collect (tstruct-slot->form slot struct-def)))

       ,@(tstruct-def->defun-functions struct-def)

       ',(tstruct-def-name struct-def))))




(defmacro transactional-struct ((defstruct name-and-options &rest slot-descriptions))
  "Define NAME as a new transactional struct.
Use this macro to wrap a normal DEFSTRUCT as follows:
\(TRANSACTIONAL-STRUCT (DEFSTRUCT {struct-name | (struct-name [options])} slots-description*)

The effect is the same as DEFSTRUCT, plus:
- by default, direct slots are transactional memory (implemented by TVARs)"

  (let ((struct-def (parse-tstruct-def name-and-options))
        (slots (parse-tstruct-slots slot-descriptions)))

    (setf (tstruct-def-slots struct-def) (remove-if #'stringp slots))

    (tstruct-def->all defstruct struct-def)))

