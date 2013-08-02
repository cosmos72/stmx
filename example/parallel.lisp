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

(defpackage #:stmx.example.parallel
  (:use #:cl
        #:bordeaux-threads
        #:sb-transaction
        #:stmx.lang
        #:stmx
        #:stmx.util))

(in-package :stmx.example.parallel)

(enable-#?-syntax)  

(defun run-one-thread (var)
  "loop invoking transactions that decrease VAR until it is zero"
  (declare (type tvar var))
  (loop until (zerop (the fixnum (atomic (decf (the fixnum ($ var))))))))


(defun run-one-thread/opt (var)
  "loop invoking transactions that decrease VAR until it is zero"
  (declare (type tvar var))
  (loop until
       (zerop
        (the fixnum
          (stmx::hw-atomic2 (helper :test-for-running-tx? nil)
           (decf (the fixnum (stmx::$-hwtx var)))
           (stmx::sw-atomic
            (decf (the fixnum (stmx::$-tx var)))))))))


(defun run-one-thread/hw-only (var)
  "loop invoking transactions that decrease VAR until it is zero"
  (declare (type tvar var))
  (loop until
       (zerop
        (the fixnum
          (if (= +transaction-started+ (transaction-begin))
              (prog1
                  (decf (the fixnum (stmx::tvar-value var)))
                (transaction-end))
              -1)))))



(defun run-threads (n-threads &optional (iterations 100000000))
  "start multiple threads, each invoking (run-one-thread) on a different TVAR"
  (declare (type fixnum n-threads iterations))

  (when (< n-threads 1)
    (error "n-threads is ~A, expecting at least 1" n-threads))

  (let* ((n n-threads)
         (vars (loop for i from 1 to n collect (tvar iterations)))
         (start (get-internal-real-time))
         (threads (loop for var in vars
                     for i from 1
                     collect
                       (let1 var var
                             (start-thread (lambda () (run-one-thread/hw-only var))
                                           :name (format nil "thread ~A" i))))))

      (loop for thread in threads do
           (let ((result (wait4-thread thread)))
             (when result
               (print result))))

      (let* ((end (get-internal-real-time))
             (elapsed-secs (/ (- end start) (float internal-time-units-per-second)))
             (elapsed-secs (max elapsed-secs 0.000000001))
             (tx-speed/thread (/ iterations elapsed-secs)))

        (format t "(:n-threads ~S :tx-speed ~,3E :tx-speed/thread ~,3E) ;;~3$ secs~%"
                  n (* n tx-speed/thread) tx-speed/thread elapsed-secs))))

#+never
(:results
 (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
       :global-clock :gv5 :algorithm '(atomic $)
  (:n-threads   1 :tx-speed  37.3e6 :tx-speed/thread 37.30e6) ;;2.681 secs
  (:n-threads   2 :tx-speed  74.6e6 :tx-speed/thread 37.31e6) ;;2.680 secs
  (:n-threads   3 :tx-speed 111.6e6 :tx-speed/thread 37.22e6) ;;2.688 secs
  (:n-threads   4 :tx-speed 148.9e6 :tx-speed/thread 37.22e6) ;;2.686 secs
  (:n-threads   5 :tx-speed 154.6e6 :tx-speed/thread 30.96e6) ;;3.234 secs
  (:n-threads   6 :tx-speed 185.8e6 :tx-speed/thread 30.96e6) ;;3.230 secs  
  (:n-threads   7 :tx-speed 209.8e6 :tx-speed/thread 29.98e6) ;;3.336 secs
  (:n-threads   8 :tx-speed 237.5e6 :tx-speed/thread 29.68e6) ;;3.369 secs
  (:n-threads   9 :tx-speed 191.5e6 :tx-speed/thread 21.28e6) ;;4.700 secs
  (:n-threads  10 :tx-speed 212.0e6 :tx-speed/thread 21.20e6) ;;4.716 secs
  (:n-threads  11 :tx-speed 202.1e6 :tx-speed/thread 18.38e6) ;;5.442 secs
  (:n-threads  12 :tx-speed 219.3e6 :tx-speed/thread 18.27e6) ;;5.473 secs
  (:n-threads  14 :tx-speed 229.9e6 :tx-speed/thread 16.42e6) ;;6.090 secs
  (:n-threads  16 :tx-speed 233.5e6 :tx-speed/thread 14.60e6) ;;6.851 secs
  (:n-threads  20 :tx-speed 231.8e6 :tx-speed/thread 11.59e6) ;;8.627 secs
  (:n-threads  24 :tx-speed 230.0e6 :tx-speed/thread  9.58e6) ;;10.436 secs
  (:n-threads  32 :tx-speed 234.4e6 :tx-speed/thread  7.32e6) ;;13.654 secs
  (:n-threads  48 :tx-speed 236.6e6 :tx-speed/thread  4.93e6) ;;20.286 secs
  (:n-threads  64 :tx-speed 236.7e6 :tx-speed/thread  3.67e6) ;;27.042 secs  
  (:n-threads  96 :tx-speed 237.2e6 :tx-speed/thread  2.47e6) ;;40.471 secs
  (:n-threads 128 :tx-speed 237.0e6 :tx-speed/thread  1.85e6));;54.014 secs

 (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
       :global-clock :gv5 :algorithm '(hw-atomic2 $-hwtx $-tx)
  (:n-threads   1 :tx-speed  44.5e6 :tx-speed/thread 44.48e6) ;;2.248 secs
  (:n-threads   2 :tx-speed  88.8e6 :tx-speed/thread 44.40e6) ;;2.252 secs
  (:n-threads   3 :tx-speed 133.4e6 :tx-speed/thread 44.46e6) ;;2.249 secs 
  (:n-threads   4 :tx-speed 177.9e6 :tx-speed/thread 44.46e6) ;;2.249 secs
  (:n-threads   5 :tx-speed 182.6e6 :tx-speed/thread 36.52e6) ;;2.738 secs 
  (:n-threads   6 :tx-speed 215.9e6 :tx-speed/thread 35.98e6) ;;2.779 secs
  (:n-threads   7 :tx-speed 251.5e6 :tx-speed/thread 35.93e6) ;;2.783 secs
  (:n-threads   8 :tx-speed 275.4e6 :tx-speed/thread 34.42e6) ;;2.905 secs
  (:n-threads   9 :tx-speed 227.4e6 :tx-speed/thread 25.27e6) ;;3.957 secs 
  (:n-threads  10 :tx-speed 249.8e6 :tx-speed/thread 24.98e6) ;;4.004 secs
  (:n-threads  11 :tx-speed 254.0e6 :tx-speed/thread 23.09e6) ;;4.331 secs 
  (:n-threads  12 :tx-speed 269.9e6 :tx-speed/thread 22.49e6) ;;4.446 secs
  (:n-threads  14 :tx-speed 268.7e6 :tx-speed/thread 19.19e6) ;;5.211 secs
  (:n-threads  16 :tx-speed 262.0e6 :tx-speed/thread 16.37e6) ;;6.108 secs
  (:n-threads  20 :tx-speed 273.7e6 :tx-speed/thread 13.68e6) ;;7.308 secs
  (:n-threads  24 :tx-speed 273.3e6 :tx-speed/thread 11.39e6) ;;8.781 secs
  (:n-threads  32 :tx-speed 274.6e6 :tx-speed/thread  8.58e6) ;;11.654 secs
  (:n-threads  48 :tx-speed 274.9e6 :tx-speed/thread  5.72e6));;17.461 secs


 (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t :global-clock :gv5
       :algorithm '(transaction-begin tvar-value transaction-end)
  (:n-threads   1 :tx-speed  51.4e6 :tx-speed/thread 51.44e6) ;;1.944 secs
  (:n-threads   2 :tx-speed 102.7e6 :tx-speed/thread 51.33e6) ;;1.948 secs
  (:n-threads   3 :tx-speed 153.9e6 :tx-speed/thread 51.31e6) ;;1.949 secs 
  (:n-threads   4 :tx-speed 204.9e6 :tx-speed/thread 51.23e6) ;;1.952 secs
  (:n-threads   5 :tx-speed 247.5e6 :tx-speed/thread 49.50e6) ;;2.020 secs 
  (:n-threads   6 :tx-speed 296.7e6 :tx-speed/thread 49.46e6) ;;2.022 secs
  (:n-threads   7 :tx-speed 345.3e6 :tx-speed/thread 49.33e6) ;;2.027 secs
  (:n-threads   8 :tx-speed 394.9e6 :tx-speed/thread 49.36e6) ;;2.026 secs 
  (:n-threads   9 :tx-speed 300.0e6 :tx-speed/thread 33.33e6) ;;3.000 secs
  (:n-threads  10 :tx-speed 327.9e6 :tx-speed/thread 32.79e6) ;;3.050 secs 
  (:n-threads  11 :tx-speed 329.4e6 :tx-speed/thread 29.95e6) ;;3.339 secs
  (:n-threads  12 :tx-speed 354.3e6 :tx-speed/thread 29.52e6) ;;3.387 secs
  (:n-threads  14 :tx-speed 389.0e6 :tx-speed/thread 27.79e6) ;;3.599 secs
  (:n-threads  16 :tx-speed 380.2e6 :tx-speed/thread 23.76e6) ;;4.208 secs
  (:n-threads  20 :tx-speed 389.4e6 :tx-speed/thread 19.47e6) ;;5.136 secs
  (:n-threads  24 :tx-speed 387.8e6 :tx-speed/thread 16.16e6) ;;6.188 secs 
  (:n-threads  32 :tx-speed 389.1e6 :tx-speed/thread 12.16e6) ;;8.225 secs
  (:n-threads  48 :tx-speed 391.4e6 :tx-speed/thread  8.16e6) ;;12.263 secs
  (:n-threads  64 :tx-speed 393.8e6 :tx-speed/thread  6.15e6) ;;16.250 secs
  (:n-threads  96 :tx-speed 394.4e6 :tx-speed/thread  4.11e6) ;;24.341 secs
  (:n-threads 128 :tx-speed 393.9e6 :tx-speed/thread  3.08e6));;32.492 secs
)