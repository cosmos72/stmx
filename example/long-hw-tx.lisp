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


(in-package :cl-user)


(defpackage #:stmx.example5
  (:use #:cl #:stmx.asm  #:stmx.lang))

(in-package :stmx.example5)


(deftype non-negative-fixnum () '(and (integer 0) fixnum))
(deftype positive-fixnum     () '(and (integer 1) fixnum))

(declaim (inline empty-tx))
(defun empty-tx (cell)
  "An empty HW transaction. Used to measure the overhead each HW transaction.
On Intel Core i7 4770 @3.5GHz, the overhead is about 11 nanoseconds."
  (declare (type cons cell))

  (when (= (transaction-begin) +transaction-started+)
    (let1 result (first cell)
      (transaction-end)
      result)))


(defun simple-loop-tx (cell)
  "An HW transaction that loops on some simple arithmetic operation.
Used to measure the maximum time a HW transaction can last and still
have a significative chance to commit.
On Intel Core i7 4770 @3.5GHz running Debian GNU/Linux 7 (x86_64) and SBCL 1.1.8
with almost no load, some typical results are
  0.1 milliseconds: maximum commit probability about 95%
  0.3 milliseconds: maximum commit probability 65% to 80%
  0.5 milliseconds: maximum commit probability 25% to 40%
  1.0 milliseconds: maximum commit probability 10% to 25%
Beyond that, maximum commit probability goes to zero very quickly."
  (declare (type cons cell))

  (when (= (transaction-begin) +transaction-started+)
    (let1 n (the positive-fixnum (first cell))
      (dotimes (i n)
        (incf (the fixnum (rest cell))))
      (transaction-end)
      n)))


(defun alloc-tx (cell)
  "At least on SBCL, trying to allocate - even ONE SINGLE cons -
inside a HW transaction appears to abort it with probability > 99.9%"
  (declare (type cons cell))

  (let1 tx-length (the positive-fixnum (first cell))

    (when (= (transaction-begin) +transaction-started+)
      (let1 result
          (loop for i from (1- tx-length) downto 0
             collect i)
        (transaction-end)
        result))))


(defun run-tx-loop (&key (tx-length 1000) (runs (ceiling 1000000000 tx-length)))
  (declare (type positive-fixnum tx-length runs))

  (let ((cell (cons 0 0))
        (start (the positive-fixnum (get-internal-real-time)))
        (commits 0)
        (aborts  0))

    (declare (type fixnum commits aborts))

    (dotimes (i runs)
      (setf (first cell) tx-length
            (rest cell)  0)

      (let1 result (alloc-tx cell)

        (if (null result)
            (incf aborts)
            (incf commits))))

    (let* ((end (the positive-fixnum (get-internal-real-time)))
           (elapsed-secs (/ (- (float end) start) internal-time-units-per-second)))

      (log:info "~A runs, tx-length ~A" runs tx-length)

      (log:info "~A commits, ~A aborts (~2$%) in ~S seconds"
                commits aborts
                (if (zerop commits)
                    100
                    (* 100 (/ (float aborts) (+ commits aborts))))
                elapsed-secs)

      (log:info "avg. time in each transaction ~S microseconds"
                (* 1000000 (/ elapsed-secs runs))))))

