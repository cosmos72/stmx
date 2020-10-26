;; -*- lisp -*-

;; This file is part of STMX
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


(in-package :stmx.asm)

(defconstant +impl-package+
  (loop :for pkg in
       #+(or x86 x86-64)           '(:sb-x86-64-asm :sb-x86-asm :sb-vm)
       #+(or arm arm64)            '(:sb-arm64-asm :sb-arm-asm :sb-vm)
       #-(or arm arm64 x86 x86-64) '(:sb-vm)
     :when (find-package pkg)
     :return pkg)
  "Designator for the SBCL internal package where we look for VOP-related symbols")

(defun symbol-name* (symbol-name)
  (declare (type (or symbol string) symbol-name))
  (if (stringp symbol-name)
      symbol-name
      (symbol-name symbol-name)))

(defun find-symbol* (symbol-name &optional (package-name +impl-package+))
  "Find and return the symbol named SYMBOL-NAME in PACKAGE"
  (declare (type (or symbol string) symbol-name))
  (let ((symbol-name (symbol-name* symbol-name)))
    (find-symbol symbol-name package-name)))

;;;; conditional compile helpers, use as follows:
;;;; #+#.(stmx.asm::compile-if-package :package-name) (form ...)
;;;; #+#.(stmx.asm::compile-if-symbol :symbol-name :package-name) (form ...)
;;;; #+#.(stmx.asm::compile-if-lisp-version>= '(1 2 13)) (form ...)
(defun compile-if (flag)
  (if flag '(:and) '(:or)))

(defun compile-if-package (package-name)
  (compile-if (find-package package-name)))

(defun compile-if-symbol (package-name symbol-name)
  (compile-if (find-symbol* symbol-name package-name)))





(defun split-string (string separator)
  (declare (type string string)
	   (type character separator))
  (loop :for beg = 0 :then (1+ end)
     :for end = (position separator string :start beg)
     :collect (subseq string beg end)
     :while end))

(defun string-to-int-list (string &optional (separator #\.))
  (declare (type string string)
	   (type character separator))
  (loop
     :for token in (split-string string separator)
     :for i = (parse-integer token :junk-allowed t)
     :while i
     :collect i))

(defun int-list>= (list1 list2)
  (declare (type list list1 list2))
  (loop
     :for n1 = (pop list1)
     :for n2 = (pop list2)
     :do
     (cond
       ((null n1) (return (null n2)))
       ((null n2) (return t))
       ((< n1 n2) (return nil))
       ((> n1 n2) (return t)))))

(defun lisp-version>= (version-int-list)
  (declare (type (or string list) version-int-list))
  (let ((current-version (string-to-int-list
			  (lisp-implementation-version)))
	(min-version (if (listp version-int-list)
			 version-int-list
			 (string-to-int-list version-int-list))))
    (int-list>= current-version min-version)))

(defun compile-if-sbcl-lacks-rtm-instructions ()
  ;; Instructions XBEGIN XEND XABORT XTEST are defined only in SBCL >= 1.3.4
  ;;
  ;; Attempts to directly inspect sbcl internals to detect whether
  ;; the instructions are defined or not are doomed to break sooner or later,
  ;; because they mess with SBCL internal implementation details
  ;; subject to change without notice.
  ;;
  ;; Thus simply check for SBCL version.
  (compile-if (not (lisp-version>= '(1 3 4)))))

(defun compile-if-sbcl-disassem<=32-bit ()
  ;; SBCL < 1.2.14 disassembler does not support instructions longer than 32 bits,
  ;; so we will have to work around it by using a prefilter
  ;; to read beyond 32 bits while disassembling
  (compile-if (not (lisp-version>= '(1 2 14)))))


;;;; new compiler intrinsic functions

(defconstant +defknown-has-overwrite-fndb-silently+
  (dolist (arg (second (sb-kernel:type-specifier (sb-int:info :function :type 'sb-c::%defknown))))
    (when (and (consp arg)
               (eq (first arg) :overwrite-fndb-silently))
      (return t))))

(defmacro defknown (&rest args)
  `(sb-c:defknown ,@args
       ,@(if +defknown-has-overwrite-fndb-silently+ '(:overwrite-fndb-silently t) ())))


