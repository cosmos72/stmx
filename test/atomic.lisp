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

(def-suite atomic-suite :in suite)
(in-suite atomic-suite)

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

(define-condition test-error (simple-error)
  ())

(test rollback
  (prog ((var (new 'tvar :value 1)))
    (handler-case
        (progn
          (atomic :id 'test-rollback
                  (setf ($ var) 2)
                  (error 'test-error :format-arguments "test-error signalled to cause rollback"))

          (fail "error signaled inside ATOMIC was not propagated to caller"))

      (test-error ()
        (is-true (= (raw-value-of var) 1))
        (is-true (= ($ var) 1))))))
