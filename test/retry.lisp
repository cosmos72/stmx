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
           (let1 x 0.0
             (declare (type single-float x))
             (dotimes (i n)
               (log:trace "         <= R")
               (setf x (take c2))
               (log:trace "  <= ~A <= R" x)
               (put c1 x)
               (log:trace "L <= ~A" x))
             (log:debug "left ~A in L" x)
             x))
      
         (f2 ()
           (let1 x 0.0
             (declare (type single-float x))
             (dotimes (i n)
               (log:trace "L =>")
               (setf x (take c1))
               (log:trace "L => ~A +>" x)
               (incf x)
               (put c2 x)
               (log:trace "     ~A => R" x))
             (log:debug "left ~A in R" x)
             x)))
    (values #'f1 #'f2)))

(defun retry-thread4 (&key (two-tokens nil) (iterations 1))
  (declare (type fixnum iterations))

  (start-multithreading)

  (let ((c1 (new 'tcell)) ;; cells have unbound value
        (c2 (new 'tcell)))

    (multiple-value-bind (f1 f2) (retry-funs iterations c1 c2)

      (let1 ths (list (start-thread f1 :name "A")
                      (start-thread f1 :name "B")
                      (start-thread f2 :name "X")
                      (start-thread f2 :name "Y"))
        (atomic
         (when two-tokens
           (put c1 0.0))
         (put c2 0.5))

        (let1 xs (loop for th in ths
                    collect (wait4-thread th))

          (values xs (atomic (list (peek c1) (peek c2)))))))))


(defun retry-thread4-test (iterations)
  (let ((expected (+ 0.5 (* 2 iterations))))

    (multiple-value-bind (xs cs) (retry-thread4 :two-tokens nil :iterations iterations)
      (destructuring-bind (x1 x2 x3 x4) xs
        (destructuring-bind (c1 c2) cs
          (is-true (= expected (max x1 x2 x3 x4)))
          (is-true (null c1))
          (is-true (= expected c2)))))

    (multiple-value-bind (xs cs) (retry-thread4 :two-tokens t :iterations iterations)
      (destructuring-bind (x1 x2 x3 x4) xs
        (destructuring-bind (c1 c2) cs
          (is-true (= (max c1 c2) (max x1 x2 x3 x4)))
          (is-true (not (null c1)))
          (is-true (not (null c2)))
          (is-true (= expected (+ c1 c2))))))))


(test retry-thread4
  (retry-thread4-test 10000))
