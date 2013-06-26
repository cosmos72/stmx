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


(in-package :cl-user)


(defpackage #:stmx.example4
  (:use #:cl #:sb-transaction  #:stmx.lang))
                
(in-package :stmx.example4)


(deftype non-negative-fixnum () '(and (integer 0) fixnum))
(deftype positive-fixnum     () '(and (integer 1) fixnum))
                
(defun run-long-hw-tx (cell)
  (declare (type cons cell))
  
  (when (= (transaction-begin) +transaction-started+)
    (dotimes (i (the fixnum (first cell)))
      (incf (the fixnum (rest cell))))
    (transaction-end)))


(defun long-hw-tx (&key (tx-length 1000) (runs (ceiling 1000000000 tx-length)))
  (declare (type positive-fixnum tx-length runs))

  (let ((cell (cons 0 0))
        (start (the positive-fixnum (get-internal-real-time)))
        (commits 0)
        (aborts  0))

    (declare (type fixnum commits aborts))

    (dotimes (i runs)
      (setf (first cell) tx-length
            (rest cell)  0)
      (run-long-hw-tx cell)

      (if (zerop (the fixnum (rest cell)))
          (incf aborts)
          (incf commits)))

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
                    
