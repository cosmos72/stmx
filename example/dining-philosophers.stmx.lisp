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

(defpackage #:stmx.example.dining-philosophers.stmx
  (:use #:cl
        #:bordeaux-threads
        #:stmx.lang
        #:stmx
        #:stmx.util)

  (:import-from #:stmx
                #:new #:try-take-$ #:try-put-$))


(in-package :stmx.example.dining-philosophers.stmx)

(enable-#?-syntax)  

(declaim (ftype (function (cons) fixnum) eat-from-plate)
         (inline
           eat-from-plate))

(defun eat-from-plate (plate)
  "Decrease by one TVAR in plate."
  (declare (type cons plate))
  (decf (the fixnum ($ (car plate)))))

(declaim (ftype (function (tvar tvar cons) fixnum) philosopher-eats
                                                   fast-philosopher-eats)
         (inline philosopher-eats
                 fast-philosopher-eats))

(defun philosopher-eats (fork1 fork2 plate)
  "Eat once. return remaining hunger"
  (declare (type tvar fork1 fork2)
           (type cons plate))
  ;; use a normal (non-transactional) counter to keep track
  ;; of retried transactions for demonstration purposes.
  (decf (the fixnum (cdr plate)))
   
  (let ((f1 (take fork1))
        (f2 (take fork2)))
    (prog1 (eat-from-plate plate)
      (put fork1 f1)
      (put fork2 f2))))


(defun fast-philosopher-eats (fork1 fork2 plate)
  "Eat once. return remaining hunger"
  (declare (type tvar fork1 fork2)
           (type cons plate))
  ;; use a normal (non-transactional) counter to keep track
  ;; of retried transactions for demonstration purposes.
  (decf (the fixnum (cdr plate)))
   
  (let ((hunger -1) ;; unknown
        (free t)
        (busy +unbound-tvar+))

    (when (eq free ($ fork1))
      (setf ($ fork1) busy)
      (when (eq free ($ fork2))
        (setf ($ fork2) busy
              hunger (eat-from-plate plate)
              ($ fork2) free))
      (setf ($ fork1) free))

    hunger))

(defun dining-philosopher (i fork1 fork2 plate)
  "Eat until not hungry anymore."
  (declare (type tvar fork1 fork2)
           (type cons plate)
           (type fixnum i))
  ;;(with-output-to-string (out)
  ;;  (let ((*standard-output* out))
  (log:trace "~A: fork1=~A fork2=~A plate=~A"
            i ($ fork1) ($ fork2) (car plate))
  ;;(sb-sprof:with-profiling
  ;;  (:max-samples 1000 :sample-interval 0.001 :report :graph
  ;;   :loop nil :show-progress t :mode :cpu)


  ;; NOTE: this simpler version works too, but allocates a closure at each iteration:
  ;; (loop until (zerop (the fixnum (atomic (philosopher-eats fork1 fork2 plate)))))

  (let1 lambda-philosopher-eats
        (lambda () (fast-philosopher-eats fork1 fork2 plate))
    (loop until (zerop (the fixnum (run-atomic lambda-philosopher-eats))))))



(defun dining-philosophers (philosophers-count &optional (philosophers-initial-hunger 10000000))
  "Prepare the table, sit the philosophers, let them eat."
  (declare (type fixnum philosophers-count philosophers-initial-hunger))

  (when (< philosophers-count 1)
    (error "philosophers-count is ~A, expecting at least 1" philosophers-count))

  (let* ((n philosophers-count)
         (nforks (max n 2))
         (forks (loop for i from 1 to nforks collect (tvar t)))
         (plates (loop for i from 1 to n collect
                      (cons (tvar philosophers-initial-hunger)
                            philosophers-initial-hunger)))
         (philosophers
          (loop for i from 1 to n collect
               (let ((fork1 (nth (1- i)         forks))
                     (fork2 (nth (mod i nforks) forks))
                     (plate (nth (1- i)         plates))
                     (j i))

                 ;; no need to make the last philospher left-handed,
                 ;; STMX orders transactional memory locations to be locked
                 ;; (when (= i n)
                 ;;   (rotatef fork1 fork2))


                 (lambda ()
                   (dining-philosopher j fork1 fork2 plate))))))

    (let* ((start (get-internal-real-time))
           (threads (loop for philosopher in philosophers
                       for i from 1
                       collect (start-thread philosopher
                                             :name (format nil "philosopher ~A" i)))))

      (loop for thread in threads do
           (let ((result (wait4-thread thread)))
             (when result
               (print result))))

      (let* ((end (get-internal-real-time))
             (elapsed-secs (/ (- end start) (float internal-time-units-per-second)))
             (elapsed-secs (max elapsed-secs 0.000000001))
             (tx-count (/ (* n philosophers-initial-hunger) elapsed-secs))
	     (tx-unit ""))

        (when (log:debug)
          (loop for (plate . fails) in plates
             for i from 1 do
               (log:debug "philosopher ~A: ~A tx successful, ~A retried"
                          i (- philosophers-initial-hunger ($ plate)) (- fails))))

	(when (>= tx-count 100000)
	  (setf tx-count (/ tx-count 1000000)
		tx-unit " millions"))
        (log:info "~3$~A transactions per second, elapsed time: ~3$ seconds"
		  tx-count tx-unit elapsed-secs)))))

