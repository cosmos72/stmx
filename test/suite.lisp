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

(in-suite suite)

(defun configure-log4cl ()
  (log:config :sane :this-console :pattern "[%D{%H:%M:%S}] [%5P] {%t} <%c{}{}{:downcase}> - %m%n"))

(test read-tvar
  (let ((log (new 'tlog))
        (var (new 'tvar :value 1)))
    (is-true (= 1 (raw-value-of var)))
    (is-true (= 1 (read-tvar var log)))
    (write-tvar var 2 log)
    (is-true (= 1 (raw-value-of var)))
    (is-true (= 2 (read-tvar var log)))))

(test valid?
  (let ((log (new 'tlog))
	(var  (new 'tvar :value 1)))
    (is-true (valid? log))
    (read-tvar var log)
    (is-true (valid? log))
    (setf (raw-value-of var) 2)
    (is-false (valid? log))
    (read-tvar var log)
    (is-false (valid? log))
    (setf (raw-value-of var) 1)
    (is-true (valid? log))))
    
(test commit
  (let ((log (new 'tlog))
	(var (new 'tvar :value 1)))
    (write-tvar var 2 log)
    (is-true (valid? log))
    (commit log)
    (is-true (= (raw-value-of var) 2))))

(test $
  (let ((log (new 'tlog))
        (var (new 'tvar :value 1)))
    (is-true (= ($ var) 1))
    (with-recording-to-tlog log
      (is-true (= ($ var) 1))
      (setf ($ var) 2)
      (is-true (= ($ var) 2))
      (is-true (= (raw-value-of var) 1))
      (is-true (valid? log))
      (is-true (commit log))
      (is-true (= (raw-value-of var) 2)))
    (is-true (= ($ var) 2))))

(test atomic
  (let1 var (new 'tvar :value 1)
    (atomic :id 'test-atomic
      (is-true (= ($ var) 1))
      (setf ($ var) 2)
      (is-true (= ($ var) 2))
      (is-true (= (raw-value-of var) 1))
      (is-true (valid? (current-tlog))))
    (is-true (= ($ var) 2))))

(test rollback
  (let1 var  (new 'tvar :value 1)
    (handler-case
        (atomic :id 'test-rollback
          (setf ($ var) 2)
          (error "simple-error signalled to cause rollback"))
      (simple-error ()
        (is-true (= (raw-value-of var) 1))
        (is-true (= ($ var) 1)))
      (t (err)
        (fail "unexpected ~A: ~A, expecting ~A" (type-of err) err 'simple-error)))))
        

(defun cell-test ()
  (let1 c (new 'cell :value 1)
    (is-true (full? c))
    (empty! c)
    (is-true (empty? c))
    (put c 2)
    (is-true (full? c))
    (is-true (= (take c) 2))
    (is-true (empty? c))))

(test cell
  (cell-test))

(test cell-atomic
  (atomic (cell-test)))


(defun retry-funs (n c1 c2)
  (declare (type fixnum n)
	   (type cell c1 c2))

  (flet ((f1 ()
	   (let1 x 0
	     (declare (type fixnum x))
	     (dotimes (i n)
	       (log:debug "putting ~A in cell c1" x)
	       (put c1 x)
	       (log:debug "taking from cell c2")
	       (setf x (take c2))
	       (log:debug "took ~A from cell c2" x)
	       (log:debug "done"))
	     x))
      
	 (f2 ()
	   (let1 x 0
	     (declare (type fixnum x))
	     (dotimes (i n)
	       (log:debug "taking from cell c1")
	       (setf x (take c1))
	       (log:debug "took ~A from cell c1" x)
	       
	       (log:debug "putting ~A in cell c2" (1+ x))
	       (put c2 (the fixnum (1+ x))))
	     (log:debug "done")
	     x)))
    (values #'f1 #'f2)))

(defun retry-test (&optional (n 1))
  (declare (type fixnum n))
  (let ((c1 (new 'cell)) ;; cells have unbound value
        (c2 (new 'cell)))

    (multiple-value-bind (f1 f2) (retry-funs n c1 c2)

      (let* ((t1 (make-thread f1 :name "t1"))
	     (t2 (make-thread f2 :name "t2"))
	     (x1 (the fixnum (join-thread t1)))
	     (x2 (the fixnum (join-thread t2))))
	(log:debug "t1 returned ~A" x1)
	(log:debug "t2 returned ~A" x2)

	(values x1 x2 (empty? c1) (empty? c2))))))


(test retry
  (let1 n 1000
    (multiple-value-bind (x1 x2 empty-c1? empty-c2?)
	(retry-test n)
      (is-true (= x1 n))
      (is-true (= x2 (1- n)))
      (is-true empty-c1?)
      (is-true empty-c2?))))
