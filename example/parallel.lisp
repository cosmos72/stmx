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
           (decf (the fixnum (stmx::$-hwtx var helper)))
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
         (threads
          (loop for var in vars
             for i from 1
             collect
               (let1 var var
                     (start-thread (lambda () (run-one-thread var))
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


'(:results
  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv6
        :algorithm '(atomic $)
   (:n-threads   1 :tx-speed  39.2e6 :tx-speed/thread 39.25e6) ;;2.548 secs
   (:n-threads   2 :tx-speed  77.8e6 :tx-speed/thread 38.93e6) ;;2.569 secs
   (:n-threads   3 :tx-speed 116.7e6 :tx-speed/thread 38.90e6) ;;2.571 secs
   (:n-threads   4 :tx-speed 152.3e6 :tx-speed/thread 38.07e6) ;;2.627 secs
   (:n-threads   5 :tx-speed 146.2e6 :tx-speed/thread 29.23e6) ;;3.421 secs
   (:n-threads   6 :tx-speed 173.6e6 :tx-speed/thread 28.94e6) ;;3.456 secs
   (:n-threads   7 :tx-speed 188.5e6 :tx-speed/thread 26.93e6) ;;3.713 secs
   (:n-threads   8 :tx-speed 208.8e6 :tx-speed/thread 26.10e6) ;;3.831 secs
   (:n-threads   9 :tx-speed 176.5e6 :tx-speed/thread 19.61e6) ;;5.100 secs
   (:n-threads  10 :tx-speed 194.1e6 :tx-speed/thread 19.41e6) ;;5.153 secs
   (:n-threads  11 :tx-speed 196.8e6 :tx-speed/thread 17.89e6) ;;5.590 secs
   (:n-threads  12 :tx-speed 208.1e6 :tx-speed/thread 17.34e6) ;;5.767 secs
   (:n-threads  14 :tx-speed 208.9e6 :tx-speed/thread 14.92e6) ;;6.702 secs
   (:n-threads  16 :tx-speed 202.9e6 :tx-speed/thread 12.68e6) ;;7.885 secs
   (:n-threads  20 :tx-speed 207.9e6 :tx-speed/thread 10.40e6) ;;9.618 secs
   (:n-threads  24 :tx-speed 206.5e6 :tx-speed/thread  8.60e6) ;;11.623 secs
   (:n-threads  32 :tx-speed 208.7e6 :tx-speed/thread  6.52e6) ;;15.330 secs
   (:n-threads  48 :tx-speed 208.3e6 :tx-speed/thread  4.34e6) ;;23.042 secs
   (:n-threads  64 :tx-speed 209.8e6 :tx-speed/thread  3.28e6) ;;30.504 secs
   (:n-threads  96 :tx-speed 209.9e6 :tx-speed/thread  2.19e6) ;;45.738 secs
   (:n-threads 128 :tx-speed 211.5e6 :tx-speed/thread  1.65e6));;60.517 secs

  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv5
        :algorithm '(atomic $)
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
        :global-clock :gv6
        :algorithm '(hw-atomic2 $-hwtx $-tx)
   (:n-threads   1 :tx-speed  44.2e6 :tx-speed/thread 44.19e6) ;;2.263 secs
   (:n-threads   2 :tx-speed  87.7e6 :tx-speed/thread 43.84e6) ;;2.284 secs
   (:n-threads   3 :tx-speed 131.1e6 :tx-speed/thread 43.71e6) ;;2.288 secs
   (:n-threads   4 :tx-speed 174.1e6 :tx-speed/thread 43.54e6) ;;2.297 secs
   (:n-threads   5 :tx-speed 176.7e6 :tx-speed/thread 35.35e6) ;;2.829 secs
   (:n-threads   6 :tx-speed 210.8e6 :tx-speed/thread 35.14e6) ;;2.846 secs
   (:n-threads   7 :tx-speed 235.2e6 :tx-speed/thread 33.60e6) ;;2.976 secs
   (:n-threads   8 :tx-speed 264.2e6 :tx-speed/thread 33.03e6) ;;3.028 secs
   (:n-threads   9 :tx-speed 216.9e6 :tx-speed/thread 24.10e6) ;;4.149 secs
   (:n-threads  10 :tx-speed 228.1e6 :tx-speed/thread 22.81e6) ;;4.384 secs
   (:n-threads  11 :tx-speed 240.5e6 :tx-speed/thread 21.87e6) ;;4.573 secs
   (:n-threads  12 :tx-speed 244.1e6 :tx-speed/thread 20.34e6) ;;4.916 secs
   (:n-threads  14 :tx-speed 250.5e6 :tx-speed/thread 17.89e6) ;;5.589 secs
   (:n-threads  16 :tx-speed 254.9e6 :tx-speed/thread 15.93e6) ;;6.276 secs
   (:n-threads  20 :tx-speed 261.1e6 :tx-speed/thread 13.06e6) ;;7.659 secs
   (:n-threads  24 :tx-speed 260.7e6 :tx-speed/thread 10.86e6) ;;9.207 secs
   (:n-threads  32 :tx-speed 264.1e6 :tx-speed/thread  8.25e6) ;;12.118 secs
   (:n-threads  48 :tx-speed 264.6e6 :tx-speed/thread  5.51e6) ;;18.139 secs
   (:n-threads  64 :tx-speed 265.3e6 :tx-speed/thread  4.15e6) ;;24.121 secs
   (:n-threads  96 :tx-speed 265.0e6 :tx-speed/thread  2.76e6) ;;36.230 secs
   (:n-threads 128 :tx-speed 265.5e6 :tx-speed/thread  2.07e6));;48.202 secs


  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv5
        :algorithm '(hw-atomic2 $-hwtx $-tx)
   (:n-threads   1 :tx-speed  45.0e6 :tx-speed/thread 45.00e6) ;;2.222 secs
   (:n-threads   2 :tx-speed  89.9e6 :tx-speed/thread 44.96e6) ;;2.226 secs
   (:n-threads   3 :tx-speed 134.6e6 :tx-speed/thread 44.86e6) ;;2.229 secs
   (:n-threads   4 :tx-speed 179.5e6 :tx-speed/thread 44.86e6) ;;2.229 secs
   (:n-threads   5 :tx-speed 188.1e6 :tx-speed/thread 37.62e6) ;;2.658 secs
   (:n-threads   6 :tx-speed 225.7e6 :tx-speed/thread 37.62e6) ;;2.658 secs
   (:n-threads   7 :tx-speed 254.5e6 :tx-speed/thread 36.36e6) ;;2.750 secs
   (:n-threads   8 :tx-speed 288.1e6 :tx-speed/thread 36.01e6) ;;2.777 secs
   (:n-threads   9 :tx-speed 232.6e6 :tx-speed/thread 25.85e6) ;;3.869 secs
   (:n-threads  10 :tx-speed 257.7e6 :tx-speed/thread 25.77e6) ;;3.880 secs
   (:n-threads  11 :tx-speed 252.9e6 :tx-speed/thread 22.99e6) ;;4.350 secs
   (:n-threads  12 :tx-speed 263.8e6 :tx-speed/thread 21.98e6) ;;4.549 secs
   (:n-threads  14 :tx-speed 286.8e6 :tx-speed/thread 20.49e6) ;;4.881 secs
   (:n-threads  16 :tx-speed 280.6e6 :tx-speed/thread 17.53e6) ;;5.703 secs
   (:n-threads  20 :tx-speed 288.7e6 :tx-speed/thread 14.44e6) ;;6.927 secs
   (:n-threads  24 :tx-speed 286.4e6 :tx-speed/thread 11.93e6) ;;8.379 secs
   (:n-threads  32 :tx-speed 288.2e6 :tx-speed/thread  9.01e6) ;;11.104 secs
   (:n-threads  48 :tx-speed 289.2e6 :tx-speed/thread  6.02e6) ;;16.599 secs
   (:n-threads  64 :tx-speed 290.0e6 :tx-speed/thread  4.53e6) ;;22.069 secs
   (:n-threads  96 :tx-speed 289.8e6 :tx-speed/thread  3.02e6) ;;33.121 secs
   (:n-threads 128 :tx-speed 290.1e6 :tx-speed/thread  2.27e6));;44.125 secs


  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :irrelevant
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