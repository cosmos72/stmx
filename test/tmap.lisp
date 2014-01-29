;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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
  (let* ((m1 (new 'tmap :pred 'fixnum<))
         (m2 (copy-gmap m1)))

    (add-to-gmap m1 1 "x" 2 "y")
    (copy-gmap-into m1 m2)

    (signals rollback-error
      (atomic
       (add-to-gmap m1 1 "z" 3 "w")
       (rem-gmap m1 2)
       ;;(format t "~&-------------~%")
       ;;(print-gmap t m1)
       (error 'rollback-error)))

    ;;(format t "~&-------------~%")
    ;;(print-gmap t m1)
    (is-equal-gmap m1 m2)
    t))

      
(test tmap-rollback
  (test-tmap-rollback))

(test tmap
  (test-rbmap-class 'tmap))

(test tmap-atomic
  (atomic (test-rbmap-class 'tmap)))

