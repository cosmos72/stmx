;; -*- lisp -*-

(in-package :stmx)

;;;; * Utilities

(defgeneric id-of (obj))

(declaim (inline ~))
(defun ~ (obj) (id-of obj))

(defun compute-id-of (obj)
  (declare (type t obj))
  (let* ((str (the string (format nil "~A" obj)))
         (beg (position #\{ str))
         (end (position #\} str)))
    (the string
      (if (and beg end)
          (subseq str (1+ beg) end)
          str))))

(let1 ids (make-hash-table :test 'eq :size 100 :weakness :key)
  (defmethod id-of (obj)
    (the string
      (or
       (gethash obj ids)
       (setf (gethash obj ids) (compute-id-of obj))))))


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
