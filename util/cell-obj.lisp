;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :stmx.util)

;;;; ** Concurrent cell implemented with a transactional object

(defvar *empty-cell* (gensym "EMPTY"))

(transactional
 (defclass cell ()
   ((value :accessor value-of
           :initarg :value
           :initform *empty-cell*))))

(transaction
 (defmethod empty? ((cell cell))
   (eq (value-of cell) *empty-cell*)))

(defmethod full? ((cell cell))
  (not (empty? cell)))

(transaction
 (defmethod empty! ((cell cell))
   "Remove value from cell. does not return any value"
   (setf (value-of cell) *empty-cell*)
   (values)))

(transaction
 (defmethod take ((cell cell))
   (if (empty? cell)
       (retry)
       (prog1 (value-of cell)
         (empty! cell)))))

(transaction
 (defmethod put ((cell cell) value)
   (if (empty? cell)
       (setf (value-of cell) value)
       (retry))))

(transaction
 (defmethod try-take.redundant-but-general ((cell cell))
   "this method shows a general technique to convert a blocking, atomic operation
into a nonblocking, atomic one: simply wrap it in (atomic (nonblocking ...))"
   (nonblocking
     (take cell))))

(transaction
 (defmethod try-put.redundant-but-general ((cell cell) val)
   "this method shows a general technique to convert a blocking, atomic operation
into a nonblocking, atomic one: simply wrap it in (atomic (nonblocking ...))"
   (nonblocking
     (put cell val))))


(transaction
 (defmethod try-take ((cell cell))
   (if (empty? cell)
       nil
       (let1 value (value-of cell)
         (empty! cell)
         (values t value)))))

(transaction
 (defmethod try-put ((cell cell) value)
   (if (empty? cell)
       (progn
         (setf (value-of cell) value)
         (values t value))
       nil)))




;; Copyright (c) 2013, Massimiliano Ghilardi
;; This file is part of STMX.
;;
;; STMX is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; STMX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with STMX. If not, see <http://www.gnu.org/licenses/>.



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
