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

(defpackage #:stmx.example1
  (:use #:cl
        #:bordeaux-threads
        #:stmx
        #:stmx.util)

  (:import-from #:stmx #:new))


(in-package :stmx.example1)
  

(declaim (ftype (function (cons) fixnum) eat-from-plate))
(declaim (inline eat-from-plate))
(defun eat-from-plate (plate)
  "Decrease by one TVAR in plate."
  (declare (type cons plate))
  (decf (the fixnum ($ (car plate)))))


(declaim (ftype (function (tvar tvar cons) fixnum) philosopher-eats))
(defun philosopher-eats (fork1 fork2 plate)
  "Eat once. return remaining hunger"
  (atomic
   ;; use a normal (non-transactional) counter to keep track
   ;; of retried transactions for demonstration purposes.
   (decf (the fixnum (cdr plate)))

   (let ((f1 (take fork1))
         (f2 (take fork2)))
     (prog1 (eat-from-plate plate)
       (put fork1 f1)
       (put fork2 f2)))))


(let ((out *standard-output*))
  (defun dining-philosopher (i fork1 fork2 plate)
    "Eat until not hungry anymore."
    (declare (type tvar fork1 fork2)
             (type cons plate)
             (type fixnum i))
    (log:info "philosopher ~A: fork1=~A fork2=~A plate=~A~%"
              i ($ fork1) ($ fork2) (car plate))
    (let ((*standard-output* out))
      ;;(sb-sprof:with-profiling
      ;;(:max-samples 1000 :sample-interval 0.001 :report :graph :loop nil :show-progress t)
        (loop while (plusp (philosopher-eats fork1 fork2 plate))))))


(defun dining-philosophers (philosophers-count &optional (philosophers-initial-hunger 1000000))
  "Prepare the table, sit the philosophers, let them eat."
  (declare (type fixnum philosophers-count philosophers-initial-hunger))

  (when (< philosophers-count 2)
    (error "philosophers-count is ~A, expecting at least 2" philosophers-count))

  (let* ((n philosophers-count)
         (forks (loop for i from 1 to n collect (make-tvar :value i)))
         (plates (loop for i from 1 to n collect
                      (cons (make-tvar :value philosophers-initial-hunger)
                            philosophers-initial-hunger)))
         (philosophers
          (loop for i from 1 to n collect
               (let ((fork1 (nth (1- i) forks))
                     (fork2 (nth (mod i n) forks))
                     (plate (nth (1- i) plates))
                     (j i))
                 (lambda ()
                   (dining-philosopher j fork1 fork2 plate))))))

    (time
     (let ((threads (loop for philosopher in philosophers
                       for i from 1
                       collect (make-thread philosopher
                                            :name (format nil "philosopher ~A" i)))))

       (loop for thread in threads do
            (join-thread thread))))

    (loop for (plate . fails) in plates
       for i from 1 do
         (format t "philosopher ~A: ~A successful transactions, ~A retried~%"
                 i (- philosophers-initial-hunger ($ plate)) (- fails)))))

    
  