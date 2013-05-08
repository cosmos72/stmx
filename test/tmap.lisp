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


(in-package :stmx.test)

(def-suite tmap-suite :in suite)
(in-suite tmap-suite)

(define-condition rollback-error (error)
  ())

(defun test-tmap-rollback ()
  (let* ((m1 (new 'tmap :pred #'fixnum<))
         (m2 (copy-bmap m1)))

    (add-to-bmap m1 1 "x" 2 "y")
    (copy-bmap-into m1 m2)

    (signals rollback-error
      (atomic
       (add-to-bmap m1 1 "z" 3 "w")
       (rem-bmap m1 2)
       ;;(format t "~&-------------~%")
       ;;(print-bmap t m1)
       (error 'rollback-error)))

    ;;(format t "~&-------------~%")
    ;;(print-bmap t m1)
    (is-equal-bmap m1 m2)
    t))

      
(test tmap-rollback
  (test-tmap-rollback))

(test tmap
  (test-rbmap-class 'tmap))

(test tmap-atomic
  (atomic (test-rbmap-class 'tmap)))

