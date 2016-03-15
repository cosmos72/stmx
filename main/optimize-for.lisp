;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013-2016 Massimiliano Ghilardi
;;
;; this library is free software: you can redistribute it and/or
;; modify it under the terms of the lisp lesser general public license
;; (http://opensource.franz.com/preamble.html), known as the llgpl.
;;
;; this library is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.
;; see the lisp lesser general public license for more details.


(in-package :stmx)

(enable-#?-syntax)

;;;; Utilities

(defun lambda-list-param-to-arg (param)
  "Given the lambda-list parameter PARAM, which can be of the forms
  arg-name
  (arg-name default-value)
  ((keyword-name arg-name) default-value)
return arg-name"
  (if (consp param)
      (let ((param (first param)))
        (if (consp param)
            (second param)
            param))
      param))

(defun lambda-list-param-to-arg-set-p (param)
  "Given the lambda-list parameter PARAM, which can be of the forms
  arg-name
  (arg-name default-value)
  (arg-name default-value arg-set-p)
  ((keyword-name arg-name) default-value)
  ((keyword-name arg-name) default-value arg-set-p)
return arg-set-p, or NIL if not present"
  (when (consp param)
    (third param)))

  
(defun lambda-list-to-args (lambda-list)
  "Remove all occurrences of &optional &key &rest &allow-other-keys and &aux from LAMBDA-LIST.
Also extract actual argument names from &optional, &key and &aux parameters.
For example, it converts:
\(arg1 &optional (arg2 default2) &rest rest &key arg3 (arg4 default4) ((:key5 arg5) default5 set5))
to
\(arg1 arg2 rest arg3 arg4 arg5 set5)"
  (let ((result))
    (dolist (param lambda-list)
      (unless (member param '(&optional &key &rest &allow-other-keys &aux))
        (push (lambda-list-param-to-arg param) result)
        (let ((param-p (lambda-list-param-to-arg-set-p param)))
          (when param-p
            (push param-p result)))))
    (nreverse result)))


(defun param-quote-default-form (param)
  "If present, quote the default value of the lambda-list parameter PARAM.
Useful to use in a macro a lambda list created for a function."
  (if (and (consp param) (rest param))
      (list* (first param) (list 'quote (second param)) (cddr param))
      param))

(defun lambda-list-quote-default-forms (lambda-list)
  "Quote all default values in LAMBDA-LIST. Useful to use in a macro
a lambda list created for a function."
  (loop for param in lambda-list
     collect (param-quote-default-form param)))




  
(defun extract-docstrings (body)
  (let ((docstrings (when (and (stringp (first body)) (rest body))
                      (list (pop body)))))
    (values docstrings body)))

(defun extract-declares (body)
  (let ((declares
         (loop for form in body
            while (and (consp form) (eq 'declare (first form)))
            collect (pop body))))
    (values declares body)))

(defmacro with-docstrings ((docstrings &optional var-body) func-body &body body)
  (if var-body
      `(multiple-value-bind (,docstrings ,var-body) (extract-docstrings ,func-body)
         ,@body)
      `(let ((,docstrings (extract-docstrings ,func-body)))
         ,@body)))
    
(defmacro with-declares ((declares &optional var-body) func-body &body body)
  (if var-body
      `(multiple-value-bind (,declares ,var-body) (extract-declares ,func-body)
         ,@body)
      `(let ((,declares (extract-declares ,func-body)))
         ,@body)))

(defmacro with-docstrings-declares ((docstrings declares &optional var-body) func-body &body body)
  (with-gensym d-body
    `(with-docstrings (,docstrings ,d-body) ,func-body
       (with-declares (,declares ,@(if var-body (list var-body) nil)) ,d-body
         ,@body))))




;;;; ** Optimizing macros

(deftype compiling-transaction () '(member :hwtx :swtx :notx nil))

(eval-always
  (declaim (type hash-table *optimized-funs*))
  (defparameter *optimized-funs* (make-hash-table :test 'equalp))

  (defun add-optimized-fun* (fun simple-vector)
    (declare (type simple-vector simple-vector))
    (setf (gethash fun *optimized-funs*) simple-vector)
    fun)

  (defun add-optimized-fun (fun fun-hwtx fun-swtx fun-notx lambda-list)
    (add-optimized-fun*
     fun (make-array 4 :initial-contents
                     (list fun-hwtx fun-swtx fun-notx
                           ;; warning: saving for future macrolets
                           ;; a lambda-list created for a function.
                           ;; we must quote all default forms!
                           (lambda-list-quote-default-forms lambda-list)))))

  (add-optimized-fun* '$ #($-hwtx $-swtx $-notx (var)))
  (add-optimized-fun* 'set-$ #(set-$-hwtx set-$-swtx set-$-notx (value var))))
  
  
(defun bind-optimized-fun (fun fun-tx lambda-list)
  "Return a form suitable for MACROLET"
  (let ((args (lambda-list-to-args lambda-list)))
    `(,fun ,lambda-list `(,',fun-tx ,,@args))))
   
(defmacro with-tx ((kind) &body body)
  (check-type kind compiling-transaction)
  (let ((index (position kind #(:hwtx :swtx :notx nil))))
    (if (= index 3)
        `(progn ,@body)
        `(macrolet ,(loop for k being the hash-keys in *optimized-funs* using (hash-value v)
                       collect (bind-optimized-fun k (svref v index) (svref v 3)))
           ,@body))))


(defmacro with-hwtx (&body body)
  `(with-tx (:hwtx) ,@body))

(defmacro with-swtx (&body body)
  `(with-tx (:swtx) ,@body))

(defmacro with-notx (&body body)
  `(with-tx (:notx) ,@body))






(defmacro options-for-defun ((&key inline (block-name nil block-name-p) defsetf-name)
                            (defun name args &body body))
  (with-docstrings-declares (docstrings declares var-body) body
    `(progn
       (declaim (,(if inline 'inline 'notinline) ,name))
       ,@(when defsetf-name
           ;; safe, and a bit paranoid..
           `((fmakunbound '(setf ,defsetf-name))
             (defsetf ,defsetf-name ,(rest args) (,(first args))
               ,@docstrings
               `(,',name ,,@args))))
       (,defun ,name ,args
         ,@docstrings
         ,@declares
         ,(if block-name-p
              `(block ,block-name ,@var-body)
              `(progn ,@var-body))))))


    

;; known options are :hwtx :swtx :notx :inline
(defmacro optimize-for-transaction* ((&key inline
                                    (body-hwtx nil body-hwtx?)
                                    (body-swtx nil body-swtx?)
                                    (body-notx nil body-notx?))
                                 (defun-or-defmethod name (&rest args) &body body))

  (check-type defun-or-defmethod (member defun defmethod))

  (when (eq 'defmethod defun-or-defmethod)
    (loop for opt in '(:body-hwtx :body-swtx :body-notx)
       for body? in (list body-hwtx? body-swtx? body-notx?)
       when body?
       do
         (compile-error
          "error compiling (OPTIMIZE-FOR-TRANSACTION* (~S ...) (DEFMETHOD ~A ~A ...)):
option ~S is supported only for DEFUN, not for DEFMETHOD"
          opt name args opt))

    (return-from optimize-for-transaction*
      `(progn
         (declaim (,(if inline 'inline 'notinline) ,name))
         (,defun-or-defmethod ,name ,args ,@body))))

  (with-docstrings (docstrings d-body) body
    (with-declares (declares n-body) d-body
      
      ;; support function names '(setf ...)
      (let* ((simple-name? (not (consp name)))
             (simple-name  (if simple-name? name (second name)))
             (infix        (if simple-name? '/ '-set/))
             (mangled-name (if simple-name? name (concat-symbols '%stmx-impl/tx infix simple-name)))
             (kinds        '(:hwtx :swtx :notx))
             (active-kinds #?+hw-transactions kinds
                           #?-hw-transactions (rest kinds))
             (funs         (loop for kind in kinds
                              collect
                                (if (member kind active-kinds)
                                    (concat-symbols '%stmx-impl/ kind infix simple-name)
                                    nil)))
             (fun-plist    (loop for kind in kinds for fun in funs
                              collect kind collect fun))
             (bodies       (list
                            (if body-hwtx? body-hwtx `((with-hwtx ,@n-body)))
                            (if body-swtx? body-swtx `((with-swtx ,@n-body)))
                            (if body-notx? body-notx `((with-notx ,@n-body)))))
             (params   (lambda-list-to-args args)))
        
        
        (flet ((call-fun (kind params)
                 (let ((fun-tx-name (getf fun-plist kind)))
                   `(,fun-tx-name ,@params))))
        
          `(progn
             ,@(loop for kind in kinds
                  for fun in funs
                  for defsetf = (if simple-name? nil
                                    (concat-symbols '%stmx-impl/ kind '/ simple-name))
                  for body in bodies
                  when (member kind active-kinds)
                  collect `(options-for-defun
                            (:inline ,inline :block-name ,simple-name :defsetf-name ,defsetf)
                            (defun ,fun ,params ,@declares ,@body)))

             (declaim (inline ,mangled-name))

             ;; macrolet used by WITH-TX cannot bind names like (setf ...)
             ;; thus we use a mangled name
             ;; and create a setf-expander on the original name
             ,@(unless simple-name?
                  ;; warning: reusing in a macro a lambda-list created for a function.
                  ;; we must quote all default forms!
                  (let ((args-quoted (lambda-list-quote-default-forms args)))
                    ;; safe, and a bit paranoid..
                    `((fmakunbound '(setf ,simple-name))
                      (defsetf ,simple-name ,(rest args-quoted) (,(first args-quoted))
                        ,@docstrings
                        `(,',mangled-name ,,@params)))))
             
             (,defun-or-defmethod ,mangled-name ,args
               ,@docstrings
               ,@declares
               
               #?+hw-transactions
               (when (/= +invalid-version+ (hw-tlog-write-version))
		 (return-from ,mangled-name ,(call-fun :hwtx params)))

	       (if (use-$-swtx?)
		   ,(call-fun :swtx params)
		   ,(call-fun :notx params)))

             (eval-always
               (add-optimized-fun* ',mangled-name #(,@funs ,args)))
             
             ',name))))))

(defmacro optimize-for-transaction ((defun-or-defmethod name (&rest args) &body body))
  `(optimize-for-transaction* () (,defun-or-defmethod ,name ,args ,@body)))

