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


(defun make-locks (n)
  (declare (type fixnum n))
  (let1 locks (make-array n)
    (dotimes (i n)
      (setf (aref locks i) (make-lock (format nil "~A" i))))
    (the simple-vector locks)))

(defun copy-shuffle-vector (vec)
  (declare (type simple-vector vec))
  (let* ((n (length vec))
         (copy (the simple-array (make-array n :initial-contents vec))))
    (loop for i from (1- n) downto 0 do
      (let1 rand (random (1+ i))

        (unless (= rand i)
          (rotatef (aref copy rand) (aref copy i)))))
    copy))

(defun run-with-locks (locks)
  (declare (type simple-vector locks))
  (let* ((acquired 0)
         (n (length locks)))
    (unwind-protect
         (progn
           (loop for i from 0 to (1- n)
              while (acquire-lock (aref locks i) nil)
              do (setf acquired (1+ i)))
                
           (let1 success (= n acquired)
             (if success
                 (log:debug "success")
                 (progn
                   (log:debug "failure at step ~A, cannot acquire lock ~A"
                              acquired (aref locks acquired))))

             success))

      (loop for i from 0 to (1- acquired)
         do (release-lock (aref locks i))))))


(defun retry-with-locks (locks)
  "Repeat calling run-with-locks until it succeeds"
  (dotimes (i 10)
    (loop for j from 0 do
         (if (run-with-locks locks)
             (progn
               (log:info "success after ~A retries" j)
               (return t))
             (sleep 0.001)))))


(defun run-threads-with-locks (n-threads &key shuffle (n-locks 100) (locks (make-locks n-locks))
                               (function #'run-with-locks))
  (let* ((shuffled-locks (loop for i from 1 to n-threads collect
                              (if shuffle
                                  (copy-shuffle-vector locks)
                                  locks)))
         (threads (loop for i from 1 to n-threads collect
                       (let1 my-locks (pop shuffled-locks)
                         (make-thread (lambda () (funcall function my-locks))
                                      :name (format nil "~A" (1- i)))))))

    (loop for thread in threads do
         (join-thread thread))))

                     