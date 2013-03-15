;; -*- lisp -*-

(in-package :stmx.util)

;;;; ** Concurrent cell implemented with a raw TVAR

;;; Max: here we could use the same trick as cell-obj.lisp:
;;; a special *empty-tvar* value to mean "cell is empty".
;;; Anyway, using tvar functions bound-$? and unbind-$ is less verbose

(defmethod empty? ((var tvar))
  (atomic
    (bound-$? var)))

(defmethod empty! ((var tvar))
  "Remove value from tvar."
  (atomic
    (unbind-$ var)))

(defmethod full? ((var tvar))
  (not (empty? var)))

(defmethod take ((var tvar))
  (atomic
    (if (empty? var)
        (retry)
        (prog1 ($ var)
          (empty! var)))))

(defmethod put ((var tvar) value)
  (atomic
    (if (empty? var)
        (setf ($ var) value)
        (retry))))

#|
(defmethod try-take ((cell cell))
  (atomic
    (nonblocking
     (take cell))))

(defmethod try-put ((cell cell) val)
  (atomic
    (nonblocking
     (put cell val))))
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
