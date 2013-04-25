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

(def-suite retry-suite :in suite)
(in-suite retry-suite)

(defun cell-test ()
  (let1 c (new 'tcell :value 1)
    (is-true (full? c))
    (empty! c)
    (is-true (empty? c))
    (put c 2)
    (is-true (full? c))
    (is-true (= (take c) 2))
    (is-true (empty? c))))

(test cell
  (cell-test))

(test cell-atomic
  (atomic (cell-test)))


(defun retry-funs (n c1 c2)
  (declare (type fixnum n)
           (type tcell c1 c2))

  (flet ((f1 ()
           (let1 x 0
             (declare (type fixnum x))
             (dotimes (i n)
               (log:debug "putting ~A in cell c1" x)
               (put c1 x)
               (log:debug "taking from cell c2")
               (setf x (take c2))
               (log:debug "took ~A from cell c2" x)
               (log:debug "done"))
             x))
      
         (f2 ()
           (let1 x 0
             (declare (type fixnum x))
             (dotimes (i n)
               (log:debug "taking from cell c1")
               (setf x (take c1))
               (log:debug "took ~A from cell c1" x)
               
               (log:debug "putting ~A in cell c2" (1+ x))
               (put c2 (the fixnum (1+ x))))
             (log:debug "done")
             (- x))))
    (values #'f1 #'f2)))

(defun retry-thread-test (&optional (n 1))
  (declare (type fixnum n))
  (let ((c1 (new 'tcell)) ;; cells have unbound value
        (c2 (new 'tcell)))

    (multiple-value-bind (f1 f2) (retry-funs n c1 c2)

      (let* ((t1 (start-thread f1 :name "A"))
             (t2 (start-thread f1 :name "B"))
             (t3 (start-thread f2 :name "C"))
             (t4 (start-thread f2 :name "D"))
             (x1 (the fixnum (wait4-thread t1)))
             (x2 (the fixnum (wait4-thread t2)))
             (x3 (the fixnum (wait4-thread t3)))
             (x4 (the fixnum (wait4-thread t4))))

        (log:debug "t1 returned ~A" x1)
        (log:debug "t2 returned ~A" x2)
        (log:debug "t3 returned ~A" x3)
        (log:debug "t4 returned ~A" x4)

        (values x1 x2 x3 x4 (empty? c1) (empty? c2))))))


(test retry
  (let1 n 1000
    (multiple-value-bind (x1 x2 x3 x4 empty-c1? empty-c2?)
        (retry-thread-test n)
      (is-true (= (* 2 n) (+ x1 x2)))
      (is-true (<= x3 0))
      (is-true (<= x4 0))
      (is-true empty-c1?)
      (is-true empty-c2?))))
