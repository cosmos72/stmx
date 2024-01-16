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

;;; cpuid intrinsic

(defknown %cpuid
    ;;arg-types
    ((unsigned-byte 32) (unsigned-byte 32))
    ;;result-type
    (values (unsigned-byte 32) (unsigned-byte 32)
            (unsigned-byte 32) (unsigned-byte 32))
    (sb-c::always-translatable))



;;; RTM (restricted transactional memory) intrinsics

(defknown %transaction-begin () (values (unsigned-byte 32) &optional)
    (sb-c::always-translatable))

(defknown %transaction-end () (values &optional)
    (sb-c::always-translatable))

(defknown %transaction-abort ((unsigned-byte 8)) (values &optional)
    (sb-c::always-translatable))

(defknown %transaction-running-p () (values boolean &optional)
    ;; do NOT add the sb-c::movable and sb-c:foldable attributes: either of them
    ;; would declare that %transaction-running-p result only depends on its arguments,
    ;; which is NOT true: it also depends on HW state.
    (sb-c::flushable sb-c::important-result sb-c::always-translatable))
