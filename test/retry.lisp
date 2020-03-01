;; -*- lisp -*-

;; This file is part of STMX.
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


(in-package :stmx.test)

(enable-#?-syntax)

(def-suite retry-suite :in suite)
(in-suite retry-suite)

(defun cell-test ()
  (let1 c (tcell 1)
    (is-true (full? c))
    (empty! c)
    (is-true (empty? c))
    (put c 2)
    (is-true (full? c))
    (is-true (= (take c) 2))
    (is-true (empty? c))))

(def-test cell (:compile-at :definition-time)
  (cell-test))

(def-test cell-atomic (:compile-at :definition-time)
  (atomic (cell-test)))


(defun retry-funs (n c1 c2)
  (declare (type fixnum n)
           (type tcell c1 c2))

  (flet ((retry-left ()
           (let1 x 0.0f0
             (declare (type single-float x))
             (dotimes (i n)
               (log.trace "         <= R")
               (setf x (take c2))
               (log.trace "  <= ~A <= R" x)
               (put c1 x)
               (log.trace "L <= ~A" x))
             (log.debug "left ~A in L" x)
             x))

         (retry-right ()
           (let1 x 0.0f0
             (declare (type single-float x))
             (dotimes (i n)
               (log.trace "L =>")
               (setf x (take c1))
               (log.trace "L => ~A +>" x)
               (incf x)
               (put c2 x)
               (log.trace "     ~A => R" x))
             (log.debug "left ~A in R" x)
             x)))
    (values #'retry-left #'retry-right)))

(defun retry-threads (&key (two-tokens nil) (thread-pairs 2) (iterations 100))
  (declare (type fixnum thread-pairs iterations))

  (start-multithreading)

  (let ((c1 (tcell)) ;; cells have unbound value
        (c2 (tcell)))

    (multiple-value-bind (f1 f2) (retry-funs iterations c1 c2)

      (let ((ths (loop for i below thread-pairs
                    collect (start-thread f1 :name (format nil "retry-left-~S" i))
                    collect (start-thread f2 :name (format nil "retry-right-~S" i)))))
        (atomic
         (when two-tokens
           (put c1 0.0f0))
         (put c2 0.5f0))

        (let1 xs (loop for th in ths
                    collect (wait4-thread th))

          (values xs (atomic (list (peek c1) (peek c2)))))))))


(defun retry-threads-test (threads iterations)
  (let* ((thread-pairs (truncate (1+ threads) 2))
         (expected (+ 0.5f0 (* thread-pairs iterations))))

    (multiple-value-bind (xs cs)
        (retry-threads :two-tokens nil :thread-pairs thread-pairs :iterations iterations)
      (destructuring-bind (c1 c2) cs
        (is-true (null c1))
        (is-true (= expected c2))
        (is-true (= expected (apply #'max xs)))))

    (multiple-value-bind (xs cs)
        (retry-threads :two-tokens t :thread-pairs thread-pairs :iterations iterations)
      (destructuring-bind (c1 c2) cs
        (is-true (not (null c1)))
        (is-true (not (null c2)))
        (is-true (= expected (+ c1 c2)))
        (is-true (= (max c1 c2) (apply #'max xs)))))))


#?+bt/make-thread
(def-test retry-threads (:compile-at :definition-time)
  #+(and sbcl (or x86 x86-64)) (retry-threads-test 8 10000)
  #-(and sbcl (or x86 x86-64)) (retry-threads-test 4 1000))
