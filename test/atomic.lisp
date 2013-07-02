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

(test rerun
  (signals rerun-error (stmx::rerun)))

(test tx-read-of
  (let ((log (make-tlog))
        (var (tvar 1)))
    (is (= 1 (raw-value-of var)))
    (is (= 1 (tx-read-of var log)))
    (tx-write-of var 2 log)
    (is (= 1 (raw-value-of var)))
    (is (= 2 (tx-read-of var log)))))

(test valid?
  (let ((log (make-tlog))
        (var  (tvar 1)))
    (is-true (valid? log))
    (tx-read-of var log)
    (is-true (valid? log))
    (setf (raw-value-of var) 2)
    (is-false (valid? log))
    (signals rerun-error
      (tx-read-of var log))
    (is-false (valid? log))
    (set-tvar-value-and-version var 1 +invalid-version+)
    (is-true (valid? log))))
    
(test commit
  (let ((log (make-tlog))
        (var (tvar 1)))
    (tx-write-of var 2 log)
    (is-true (valid? log))
    (is-true (commit log))
    (is (= 2 (raw-value-of var)))))

(test $
  (let ((log (make-tlog))
        (var (tvar 1)))
    (is (= 1 ($ var)))
    (with-recording-to-tlog log
      (is (= 1 ($ var)))
      (is (= 2 (setf ($ var) 2)))
      (is (= 2 ($ var)))
      (is (= 1 (raw-value-of var)))
      (is-true (valid? log))
      (is-true (commit log))
      (is (= 2 (raw-value-of var))))
    (is (= 2 ($ var)))))

(test atomic
  (let ((var (tvar 0)))

    (atomic
      (is (= 0 (raw-value-of var)))
      (is (= 0 ($ var)))
      (setf ($ var) 1)
      (is (= 0 (raw-value-of var)))
      (is-true (valid? (current-tlog))))

    (is (= 1 ($ var)))))





(define-condition test-error (simple-error)
  ())

(test rollback
  (let1 var (tvar 1)
    
    (signals test-error
      (atomic
       (setf ($ var) 2)
       (error 'test-error :format-arguments "test-error signalled to cause rollback"))
      
      (fail "error signaled inside ATOMIC was not propagated to caller"))

    (is (= (raw-value-of var) 1))
    (is (= ($ var) 1))))


(transaction
 (defun transaction-return-from-setf (var value &key return-from?)
   (declare (type tvar var)
            (type boolean return-from?))
   (when return-from?
     (return-from transaction-return-from-setf (setf ($ var) value)))
   (setf ($ var) value)))
     
     

(test transaction-return-from
  (let1 var (tvar 1)
    
    (transaction-return-from-setf var 2 :return-from? nil)
    (is (= 2 ($ var)))

    (transaction-return-from-setf var 3 :return-from? t)
    (is (= 3 ($ var)))))

    
  