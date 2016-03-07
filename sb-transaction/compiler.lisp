;; -*- lisp -*-

;; This file is part of SB-TRANSACTION.
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


(in-package :sb-transaction)

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

;;;; conditional compile helper, use as follows:
;;;; #+#.(sb-transaction::compile-if-package :package-name) (form ...)
;;;; #+#.(sb-transaction::compile-if-lisp-version>= '(1 2 13)) (form ...)
(defun compile-if (flag)
  (if flag '(:and) '(:or)))

(defun compile-if-package (package-name)
  (compile-if (find-package package-name)))

(defun compile-if-symbol (package-name symbol-name)
  (let ((symbol-name (if (stringp symbol-name)
                         symbol-name
                         (symbol-name symbol-name))))
    (compile-if (find-symbol symbol-name package-name))))

(defun lisp-version>= (version-int-list)
  (declare (type (or string list) version-int-list))
  (let ((current-version (string-to-int-list
			  (lisp-implementation-version)))
	(min-version (if (listp version-int-list)
			 version-int-list
			 (string-to-int-list version-int-list))))
    (int-list>= current-version min-version)))

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


;;; cpuid intrinsic

(defknown %cpuid
    ;;arg-types
    ((unsigned-byte 32) (unsigned-byte 32))
    ;;result-type
    (values (unsigned-byte 32) (unsigned-byte 32)
            (unsigned-byte 32) (unsigned-byte 32))
    (sb-c::always-translatable))



;;; RTM (restricted transactional memory) intrinsics

(defknown %transaction-begin () (unsigned-byte 32)
    (sb-c::always-translatable))

(defknown %transaction-end () (values)
    (sb-c::always-translatable))

(defknown %transaction-abort ((unsigned-byte 8)) (values)
    (sb-c::always-translatable))

(defknown %transaction-running-p () boolean
    ;; do NOT add the sb-c::movable and sb-c:foldable attributes: either of them
    ;; would declare that %transaction-running-p result only depends on its arguments,
    ;; which is NOT true: it also depends on HW state.
    (sb-c::flushable sb-c::important-result sb-c::always-translatable))
