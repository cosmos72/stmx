;; -*- lisp -*-

(in-package :stmx)

;;;; ** Concurrent cell

(defvar *empty-cell* (gensym "EMPTY"))

(transactional
 (defclass cell ()
   ((value :accessor value-of
           :initarg :value
           :initform *empty-cell*))))

#|
(deftransaction empty? ((cell cell))
  (eq (value-of cell) *empty-cell*))

(deftransaction empty! ((cell cell))
  (setf (value-of cell) *empty-cell*))

(deftransaction take ((cell cell))
  (if (empty? cell)
      (retry)
      (prog1 (value-of cell)
        (empty! cell))))

(deftransaction put ((cell cell) val)
  (if (not (empty? cell))
      (retry)
      (setf (value-of cell) val)))

(deftransaction try-take ((cell cell))
  (nob (take cell)))

(deftransaction try-put ((cell cell) val)
  (nob (put cell val)))
|#


;; Copyright (c) 2006 Hoan Ton-That
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
