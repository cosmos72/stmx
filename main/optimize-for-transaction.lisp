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


(defmacro transaction ((defun-or-defmethod name (&rest args) &body body))
  (let ((docstrings nil))
    (if (and (stringp (first body)) (rest body))
        (setf docstrings (list (pop body))))

    (when (or (not (eq 'defun defun-or-defmethod))
              (intersection args '(&whole &optional &key &rest &allow-other-keys &aux)))
      (return-from transaction
        `(,defun-or-defmethod ,name ,args ,@docstrings (atomic ,@body))))

    (let ((fun-hwtx  (gensym "FUN-HWTX"))
          (fun-swtx  (gensym "FUN-SWTX"))
          (fun-notx  (gensym "FUN-NOTX"))
          (form      (gensym "FORM"))
          (compiling (gensym "COMPILING"))
          (tx-arg    (gensym "TX-ARG")))
      `(progn
         (compile-for-tx :hwtx (defun ,fun-hwtx ,args ,@body))
         (compile-for-tx :swtx (defun ,fun-swtx ,args ,@body))
         (compile-for-tx :notx (defun ,fun-notx ,args ,@body))
         (,defun-or-defmethod ,name ,args
           ,@docstrings
           (let ((,tx-arg *hwtx*))
             (if (/= ,tx-arg -2)
                 (,fun-hwtx ,tx-arg ,@args)
                 (let ((,tx-arg *tlog*))
                   (if ,tx-arg
                       (,fun-swtx ,tx-arg ,@args)
                       (atomic ,fun-swtx ,tx-arg ,@args))))))
         (,defun-or-defmethod ,name ,args (atomic ,@body))
         (define-compiler-macro ,name (&whole ,form ,@args)
           (print *compiling-transaction*)
           (let ((,compiling (car *compiling-transaction*))
                 (,tx-arg    (cdr *compiling-transaction*)))
             (case ,compiling
               (:hwtx     `(,,fun-hwtx ,,tx-arg ,,@args))
               (:swtx     `(,,fun-swtx ,,tx-arg ,,@args))
               (:notx     `(,,fun-notx          ,,@args))
               (otherwise form))))
         ',name))))
