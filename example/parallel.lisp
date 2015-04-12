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
          (stmx::hw-atomic2 (hwtx-version :test-for-running-tx? nil)
           (decf (the fixnum (stmx::$-hwtx hwtx-version var)))
           (stmx::sw-atomic
            (decf (the fixnum (stmx::$-swtx (stmx::current-tlog) var)))))))))


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
             (elapsed-secs (max elapsed-secs 1f-9))
             (tx-speed/thread (/ iterations elapsed-secs)))

        (format t "(:n-threads ~S :tx-speed ~,3E :tx-speed/thread ~,3E) ;;~3$ secs~%"
                  n (* n tx-speed/thread) tx-speed/thread elapsed-secs))))


'(:results
  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv6
        :algorithm '(atomic $)
   (:n-threads   1 :tx-speed  39.2f6 :tx-speed/thread 39.25f6) ;;2.548 secs
   (:n-threads   2 :tx-speed  77.8f6 :tx-speed/thread 38.93f6) ;;2.569 secs
   (:n-threads   3 :tx-speed 116.7f6 :tx-speed/thread 38.90f6) ;;2.571 secs
   (:n-threads   4 :tx-speed 152.3f6 :tx-speed/thread 38.07f6) ;;2.627 secs
   (:n-threads   5 :tx-speed 146.2f6 :tx-speed/thread 29.23f6) ;;3.421 secs
   (:n-threads   6 :tx-speed 173.6f6 :tx-speed/thread 28.94f6) ;;3.456 secs
   (:n-threads   7 :tx-speed 188.5f6 :tx-speed/thread 26.93f6) ;;3.713 secs
   (:n-threads   8 :tx-speed 208.8f6 :tx-speed/thread 26.10f6) ;;3.831 secs
   (:n-threads   9 :tx-speed 176.5f6 :tx-speed/thread 19.61f6) ;;5.100 secs
   (:n-threads  10 :tx-speed 194.1f6 :tx-speed/thread 19.41f6) ;;5.153 secs
   (:n-threads  11 :tx-speed 196.8f6 :tx-speed/thread 17.89f6) ;;5.590 secs
   (:n-threads  12 :tx-speed 208.1f6 :tx-speed/thread 17.34f6) ;;5.767 secs
   (:n-threads  14 :tx-speed 208.9f6 :tx-speed/thread 14.92f6) ;;6.702 secs
   (:n-threads  16 :tx-speed 202.9f6 :tx-speed/thread 12.68f6) ;;7.885 secs
   (:n-threads  20 :tx-speed 207.9f6 :tx-speed/thread 10.40f6) ;;9.618 secs
   (:n-threads  24 :tx-speed 206.5f6 :tx-speed/thread  8.60f6) ;;11.623 secs
   (:n-threads  32 :tx-speed 208.7f6 :tx-speed/thread  6.52f6) ;;15.330 secs
   (:n-threads  48 :tx-speed 208.3f6 :tx-speed/thread  4.34f6) ;;23.042 secs
   (:n-threads  64 :tx-speed 209.8f6 :tx-speed/thread  3.28f6) ;;30.504 secs
   (:n-threads  96 :tx-speed 209.9f6 :tx-speed/thread  2.19f6) ;;45.738 secs
   (:n-threads 128 :tx-speed 211.5f6 :tx-speed/thread  1.65f6));;60.517 secs

  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv5
        :algorithm '(atomic $)
   (:n-threads   1 :tx-speed  37.3f6 :tx-speed/thread 37.30f6) ;;2.681 secs
   (:n-threads   2 :tx-speed  74.6f6 :tx-speed/thread 37.31f6) ;;2.680 secs
   (:n-threads   3 :tx-speed 111.6f6 :tx-speed/thread 37.22f6) ;;2.688 secs
   (:n-threads   4 :tx-speed 148.9f6 :tx-speed/thread 37.22f6) ;;2.686 secs
   (:n-threads   5 :tx-speed 154.6f6 :tx-speed/thread 30.96f6) ;;3.234 secs
   (:n-threads   6 :tx-speed 185.8f6 :tx-speed/thread 30.96f6) ;;3.230 secs  
   (:n-threads   7 :tx-speed 209.8f6 :tx-speed/thread 29.98f6) ;;3.336 secs
   (:n-threads   8 :tx-speed 237.5f6 :tx-speed/thread 29.68f6) ;;3.369 secs
   (:n-threads   9 :tx-speed 191.5f6 :tx-speed/thread 21.28f6) ;;4.700 secs
   (:n-threads  10 :tx-speed 212.0f6 :tx-speed/thread 21.20f6) ;;4.716 secs
   (:n-threads  11 :tx-speed 202.1f6 :tx-speed/thread 18.38f6) ;;5.442 secs
   (:n-threads  12 :tx-speed 219.3f6 :tx-speed/thread 18.27f6) ;;5.473 secs
   (:n-threads  14 :tx-speed 229.9f6 :tx-speed/thread 16.42f6) ;;6.090 secs
   (:n-threads  16 :tx-speed 233.5f6 :tx-speed/thread 14.60f6) ;;6.851 secs
   (:n-threads  20 :tx-speed 231.8f6 :tx-speed/thread 11.59f6) ;;8.627 secs
   (:n-threads  24 :tx-speed 230.0f6 :tx-speed/thread  9.58f6) ;;10.436 secs
   (:n-threads  32 :tx-speed 234.4f6 :tx-speed/thread  7.32f6) ;;13.654 secs
   (:n-threads  48 :tx-speed 236.6f6 :tx-speed/thread  4.93f6) ;;20.286 secs
   (:n-threads  64 :tx-speed 236.7f6 :tx-speed/thread  3.67f6) ;;27.042 secs
   (:n-threads  96 :tx-speed 237.2f6 :tx-speed/thread  2.47f6) ;;40.471 secs
   (:n-threads 128 :tx-speed 237.0f6 :tx-speed/thread  1.85f6));;54.014 secs

  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv6
        :algorithm '(hw-atomic2 $-hwtx $-swtx)
   (:n-threads   1 :tx-speed  44.2f6 :tx-speed/thread 44.19f6) ;;2.263 secs
   (:n-threads   2 :tx-speed  87.7f6 :tx-speed/thread 43.84f6) ;;2.284 secs
   (:n-threads   3 :tx-speed 131.1f6 :tx-speed/thread 43.71f6) ;;2.288 secs
   (:n-threads   4 :tx-speed 174.1f6 :tx-speed/thread 43.54f6) ;;2.297 secs
   (:n-threads   5 :tx-speed 176.7f6 :tx-speed/thread 35.35f6) ;;2.829 secs
   (:n-threads   6 :tx-speed 210.8f6 :tx-speed/thread 35.14f6) ;;2.846 secs
   (:n-threads   7 :tx-speed 235.2f6 :tx-speed/thread 33.60f6) ;;2.976 secs
   (:n-threads   8 :tx-speed 264.2f6 :tx-speed/thread 33.03f6) ;;3.028 secs
   (:n-threads   9 :tx-speed 216.9f6 :tx-speed/thread 24.10f6) ;;4.149 secs
   (:n-threads  10 :tx-speed 228.1f6 :tx-speed/thread 22.81f6) ;;4.384 secs
   (:n-threads  11 :tx-speed 240.5f6 :tx-speed/thread 21.87f6) ;;4.573 secs
   (:n-threads  12 :tx-speed 244.1f6 :tx-speed/thread 20.34f6) ;;4.916 secs
   (:n-threads  14 :tx-speed 250.5f6 :tx-speed/thread 17.89f6) ;;5.589 secs
   (:n-threads  16 :tx-speed 254.9f6 :tx-speed/thread 15.93f6) ;;6.276 secs
   (:n-threads  20 :tx-speed 261.1f6 :tx-speed/thread 13.06f6) ;;7.659 secs
   (:n-threads  24 :tx-speed 260.7f6 :tx-speed/thread 10.86f6) ;;9.207 secs
   (:n-threads  32 :tx-speed 264.1f6 :tx-speed/thread  8.25f6) ;;12.118 secs
   (:n-threads  48 :tx-speed 264.6f6 :tx-speed/thread  5.51f6) ;;18.139 secs
   (:n-threads  64 :tx-speed 265.3f6 :tx-speed/thread  4.15f6) ;;24.121 secs
   (:n-threads  96 :tx-speed 265.0f6 :tx-speed/thread  2.76f6) ;;36.230 secs
   (:n-threads 128 :tx-speed 265.5f6 :tx-speed/thread  2.07f6));;48.202 secs


  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :gv5
        :algorithm '(hw-atomic2 $-hwtx $-swtx)
   (:n-threads   1 :tx-speed  45.0f6 :tx-speed/thread 45.00f6) ;;2.222 secs
   (:n-threads   2 :tx-speed  89.9f6 :tx-speed/thread 44.96f6) ;;2.226 secs
   (:n-threads   3 :tx-speed 134.6f6 :tx-speed/thread 44.86f6) ;;2.229 secs
   (:n-threads   4 :tx-speed 179.5f6 :tx-speed/thread 44.86f6) ;;2.229 secs
   (:n-threads   5 :tx-speed 188.1f6 :tx-speed/thread 37.62f6) ;;2.658 secs
   (:n-threads   6 :tx-speed 225.7f6 :tx-speed/thread 37.62f6) ;;2.658 secs
   (:n-threads   7 :tx-speed 254.5f6 :tx-speed/thread 36.36f6) ;;2.750 secs
   (:n-threads   8 :tx-speed 288.1f6 :tx-speed/thread 36.01f6) ;;2.777 secs
   (:n-threads   9 :tx-speed 232.6f6 :tx-speed/thread 25.85f6) ;;3.869 secs
   (:n-threads  10 :tx-speed 257.7f6 :tx-speed/thread 25.77f6) ;;3.880 secs
   (:n-threads  11 :tx-speed 252.9f6 :tx-speed/thread 22.99f6) ;;4.350 secs
   (:n-threads  12 :tx-speed 263.8f6 :tx-speed/thread 21.98f6) ;;4.549 secs
   (:n-threads  14 :tx-speed 286.8f6 :tx-speed/thread 20.49f6) ;;4.881 secs
   (:n-threads  16 :tx-speed 280.6f6 :tx-speed/thread 17.53f6) ;;5.703 secs
   (:n-threads  20 :tx-speed 288.7f6 :tx-speed/thread 14.44f6) ;;6.927 secs
   (:n-threads  24 :tx-speed 286.4f6 :tx-speed/thread 11.93f6) ;;8.379 secs
   (:n-threads  32 :tx-speed 288.2f6 :tx-speed/thread  9.01f6) ;;11.104 secs
   (:n-threads  48 :tx-speed 289.2f6 :tx-speed/thread  6.02f6) ;;16.599 secs
   (:n-threads  64 :tx-speed 290.0f6 :tx-speed/thread  4.53f6) ;;22.069 secs
   (:n-threads  96 :tx-speed 289.8f6 :tx-speed/thread  3.02f6) ;;33.121 secs
   (:n-threads 128 :tx-speed 290.1f6 :tx-speed/thread  2.27f6));;44.125 secs


  (:cpu 'intel-core-i7-4770 :cores 4 :hyper-threading t
        :global-clock :irrelevant
        :algorithm '(transaction-begin tvar-value transaction-end)
   (:n-threads   1 :tx-speed  51.4f6 :tx-speed/thread 51.44f6) ;;1.944 secs
   (:n-threads   2 :tx-speed 102.7f6 :tx-speed/thread 51.33f6) ;;1.948 secs
   (:n-threads   3 :tx-speed 153.9f6 :tx-speed/thread 51.31f6) ;;1.949 secs 
   (:n-threads   4 :tx-speed 204.9f6 :tx-speed/thread 51.23f6) ;;1.952 secs
   (:n-threads   5 :tx-speed 247.5f6 :tx-speed/thread 49.50f6) ;;2.020 secs 
   (:n-threads   6 :tx-speed 296.7f6 :tx-speed/thread 49.46f6) ;;2.022 secs
   (:n-threads   7 :tx-speed 345.3f6 :tx-speed/thread 49.33f6) ;;2.027 secs
   (:n-threads   8 :tx-speed 394.9f6 :tx-speed/thread 49.36f6) ;;2.026 secs 
   (:n-threads   9 :tx-speed 300.0f6 :tx-speed/thread 33.33f6) ;;3.000 secs
   (:n-threads  10 :tx-speed 327.9f6 :tx-speed/thread 32.79f6) ;;3.050 secs 
   (:n-threads  11 :tx-speed 329.4f6 :tx-speed/thread 29.95f6) ;;3.339 secs
   (:n-threads  12 :tx-speed 354.3f6 :tx-speed/thread 29.52f6) ;;3.387 secs
   (:n-threads  14 :tx-speed 389.0f6 :tx-speed/thread 27.79f6) ;;3.599 secs
   (:n-threads  16 :tx-speed 380.2f6 :tx-speed/thread 23.76f6) ;;4.208 secs
   (:n-threads  20 :tx-speed 389.4f6 :tx-speed/thread 19.47f6) ;;5.136 secs
   (:n-threads  24 :tx-speed 387.8f6 :tx-speed/thread 16.16f6) ;;6.188 secs 
   (:n-threads  32 :tx-speed 389.1f6 :tx-speed/thread 12.16f6) ;;8.225 secs
   (:n-threads  48 :tx-speed 391.4f6 :tx-speed/thread  8.16f6) ;;12.263 secs
   (:n-threads  64 :tx-speed 393.8f6 :tx-speed/thread  6.15f6) ;;16.250 secs
   (:n-threads  96 :tx-speed 394.4f6 :tx-speed/thread  4.11f6) ;;24.341 secs
   (:n-threads 128 :tx-speed 393.9f6 :tx-speed/thread  3.08f6));;32.492 secs
)
