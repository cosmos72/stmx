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

(let ((not-found (gensym (symbol-name 'not-found-))))
  (defun instruction-defined? (inst-name &optional (package-name +impl-package+))
    (block nil
      ;; recent SBCL versions annotate lowercase instructions symbols
      ;; with sb-disassem:instruction-flavors
      #+#.(stmx.asm:compile-if-symbol :sb-disassem :instruction-flavors)
      (let* ((inst-name (string-downcase (symbol-name* inst-name)))
             (inst (find-symbol* inst-name package-name)))
        (when
            (and inst
                 (not (eq not-found
                          (get inst 'sb-disassem::instruction-flavors not-found))))
          (return t)))

      ;; instead old SBCL versions use hash-table sb-assem:*assem-instructions*
      ;;
      ;; but we actually don't care: on such old versions we can always
      ;; define CPU instructions, as we have all the necessary tools:
      ;; SB-DISASSEM:DEFINE-INSTRUCTION-FORMAT and SB-VM:DEFINE-INSTRUCTION
      #+#.(stmx.asm:compile-if-symbol :sb-assem :*assem-instructions*)
      #-(and)
      (let ((table sb-assem:*assem-instructions*))
        ;; if table is empty, it is unused -> ignore it
        (unless (zerop (hash-table-count table))
          (let ((inst-name (symbol-name* inst-name)))
            (when (nth-value 1 (gethash inst-name table))
              (return t)))))

      nil)))



(defun compile-if-instruction-defined (inst-name)
  (compile-if (instruction-defined? inst-name)))
