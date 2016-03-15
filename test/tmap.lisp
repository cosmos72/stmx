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

(def-suite tmap-suite :in suite)
(in-suite tmap-suite)

(define-condition rollback-error (error)
  ())

(defun test-tmap-rollback ()
  (let* ((m1 (new 'tmap :pred 'fixnum<))
         (m2 (copy-gmap m1)))

    (add-to-gmap m1 1 "x" 2 "y")
    (copy-gmap-into m1 m2)

    (signals rollback-error
      (atomic
       (add-to-gmap m1 1 "z" 3 "w")
       (rem-gmap m1 2)
       ;;(format t "~&-------------~%")
       ;;(print-gmap t m1)
       (error 'rollback-error)))

    ;;(format t "~&-------------~%")
    ;;(print-gmap t m1)
    (is-equal-gmap m1 m2)
    t))


(def-test tmap-rollback (:compile-at :definition-time)
  (test-tmap-rollback))
      

(def-test tmap (:compile-at :definition-time)
  (test-rbmap-class 'tmap))

(def-test tmap-atomic (:compile-at :definition-time)
  (atomic (test-rbmap-class 'tmap)))



#?+bt/make-thread
(defun tmap-insert-func (m iterations)
  (declare (type gmap m)
           (type fixnum iterations))
  (let ((self (bt:thread-name (bt:current-thread)))
        (max -1))
    (dotimes (i iterations max)
      (let ((result
             (atomic
              (multiple-value-bind (key value present) (max-gmap m)
                (declare (ignore value))
                (unless present (setf key max))
                (set-gmap m (incf (the fixnum key)) self)
                key))))
        (setf max (max result max))))))


#?+bt/make-thread
(defun tmap-remove-func (m iterations)
  (declare (type gmap m)
           (type fixnum iterations))
  (let ((max -1))
    (dotimes (i iterations max)
      (let ((result
             (atomic
              (multiple-value-bind (key value present) (min-gmap m)
                (declare (ignore value))
                (unless present (retry))
                (rem-gmap m key)
                key))))
        (setf max (max result max))))))


  
#?+bt/make-thread
(defun test-tmap-threads (&key (thread-pairs 4)
			    (iterations #+x86-64 1000 #-x86-64 100))
  (declare (type fixnum thread-pairs iterations))

  (start-multithreading)

  (let ((m (new 'tmap :pred 'fixnum<)))
    
    (flet ((tmap-insert ()
             (tmap-insert-func m iterations))
           (tmap-remove ()
             (tmap-remove-func m iterations)))

      (let ((threads (loop for i below thread-pairs
                        collect (start-thread #'tmap-insert :name (format nil "tmap-insert-~S" i))
                        collect (start-thread #'tmap-remove :name (format nil "tmap-remove-~S" i)))))

        (loop for thread in threads
           do (let ((x (wait4-thread thread)))
                (log:debug "thread ~A returned ~A" (thread-name thread) x)
                x))
        (when (boundp 'fiveam::current-test)
          (is (= 0 (gmap-count m))))))))


#?+bt/make-thread
(def-test tmap-threads (:compile-at :definition-time)
  (test-tmap-threads))

