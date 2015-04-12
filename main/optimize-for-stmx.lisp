;; -*- lisp -*-

;; this file is part of stmx.
;; copyright (c) 2013-2014 Massimiliano Ghilardi
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
    (setf (get-hash *optimized-funs* fun) simple-vector)
    fun)

  (defun add-optimized-fun (fun fun-hwtx fun-swtx fun-notx lambda-list)
    (add-optimized-fun* fun (make-array 4 :initial-contents
                                        (list fun-hwtx fun-swtx fun-notx lambda-list))))

  (add-optimized-fun* '$ #($-hwtx $-swtx $-notx (var)))
  (add-optimized-fun* 'set-$ #(set-$-hwtx set-$-swtx set-$-notx (value var))))
  
  
(defun bind-optimized-fun (fun fun-tx lambda-list)
  "Return a form suitable for MACROLET"
  (let ((args (lambda-list-to-args lambda-list)))
    ;; warning: reusing in a macro a lambda-list created for a function.
    ;; we must quote all default forms!
    `(,fun ,(lambda-list-quote-default-forms lambda-list)
           `(,',fun-tx ,,@args))))
   
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






    

(defmacro optimize-for-tx ((kind &optional
                                (simple-name nil simple-name-p) inline)
                                  (defun name args &body body))
  (with-docstrings-declares (docstrings declares var-body) body
    (let ((tx-body `(with-tx (,kind) ,@var-body)))
      `(progn
         (declaim (,(if inline 'inline 'notinline) ,name))
         (,defun ,name ,args
           ,@docstrings
           ,@declares
           ,(if simple-name-p
                `(block ,simple-name ,tx-body)
                `(progn ,tx-body)))))))


(defmacro optimize-for-stmx* ((&key inline &allow-other-keys)
                                    (defun-or-defmethod name (&rest args) &body body))

  (unless (eq 'defun defun-or-defmethod)
    (return-from optimize-for-stmx*
      `(progn
         ,@(when inline `((declaim (inline ,name))))
         (,defun-or-defmethod ,name ,args ,@body))))

  (with-docstrings (docstrings d-body) body
    (with-declares (declares) d-body
      
      ;; support function names '(setf ...)
      (let* ((simple-name? (not (consp name)))
             (simple-name  (if simple-name? name (second name)))
             (infix        (if simple-name? '/ '-set/))
             (mangled-name (if simple-name? name (concat-symbols '%stmx-impl/tx infix simple-name)))
             (fun-hwtx (concat-symbols '%stmx-impl/hwtx infix simple-name))
             (fun-swtx (concat-symbols '%stmx-impl/swtx infix simple-name))
             (fun-notx (concat-symbols '%stmx-impl/notx infix simple-name))
             (inline   (if inline :inline nil))
             (params   (lambda-list-to-args args)))

        (flet ((call-fun (kind params)
                 (let ((fun-tx-name (getf `(:hwtx ,fun-hwtx
                                            :swtx ,fun-swtx
                                            :notx ,fun-notx)
                                          kind)))
                   `(,fun-tx-name ,@params))))
        
          `(progn
             #?+hw-transactions
             (optimize-for-tx (:hwtx ,simple-name ,inline) (defun ,fun-hwtx ,params ,@d-body))
             (optimize-for-tx (:swtx ,simple-name ,inline) (defun ,fun-swtx ,params ,@d-body))
             (optimize-for-tx (:notx ,simple-name ,inline) (defun ,fun-notx ,params ,@d-body))

             (declaim (inline ,mangled-name))
             (,defun-or-defmethod ,mangled-name ,args
               ,@docstrings
               ,@declares
               
               #?+hw-transactions
               (when (/= +invalid-version+ (hw-tlog-write-version))
		 (return-from ,mangled-name ,(call-fun :hwtx params)))

	       (if (use-$-swtx?)
		   ,(call-fun :swtx params)
		   ,(call-fun :notx params)))

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
                        `(,',mangled-name ,,@params)))))
             (eval-always
               (add-optimized-fun* ',mangled-name #(,fun-hwtx ,fun-swtx ,fun-notx ,args)))
             
             ',name))))))

(defmacro optimize-for-stmx ((defun-or-defmethod name (&rest args) &body body))
  `(optimize-for-stmx* () (,defun-or-defmethod ,name ,args ,@body)))

(defmacro optimize-for-stmx-inline ((defun-or-defmethod name (&rest args) &body body))
  `(optimize-for-stmx* (:inline t) (,defun-or-defmethod ,name ,args ,@body)))


