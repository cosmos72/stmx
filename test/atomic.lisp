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


(in-package :stmx.test)

(def-suite atomic-suite :in suite)
(in-suite atomic-suite)

(def-test rerun (:compile-at :definition-time)
  (signals rerun-error (stmx::rerun)))

(def-test tx-read-of (:compile-at :definition-time)
  (let ((log (make-tlog))
        (var (tvar 1)))
    (is (= 1 (raw-value-of var)))
    (is (= 1 (tx-read-of var log)))
    (tx-write-of 2 var log)
    (is (= 1 (raw-value-of var)))
    (is (= 2 (tx-read-of var log)))))

(defun valid?-test ()
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

(def-test valid? (:compile-at :definition-time)
  (valid?-test))

(defun commit-test ()
  (let ((log (make-tlog))
        (var (tvar 1)))
    (tx-write-of 2 var log)
    (is-true (valid? log))
    (is-true (commit log))
    (is (= 2 (raw-value-of var)))))

(def-test commit (:compile-at :definition-time)
  (commit-test))

(defun $-test ()
  (let ((var (tvar 1))
        (log (make-tlog)))
        
    (is (= 1 ($ var)))
    (is (= 1 (raw-value-of var)))
    (with-recording-to-tlog log
      ;; global-clock GV5 used by hardware transactions
      ;; causes 50% of software transactions to abort... work around it
      (setf (stmx::tlog-read-version log) (stmx::tvar-version var))

      (is (= 1 ($ var)))
      (is (= 1 (raw-value-of var)))
      
      (is (= 2 (setf ($ var) 2)))
      (is (= 2 ($ var)))
      (is (= 1 (raw-value-of var)))

      (is (= 3 (setf ($ var) 3)))
      (is (= 3 ($ var)))
      (is (= 1 (raw-value-of var)))

      (is-true (valid? log))
      (is-true (commit log))
      (is (= 3 (raw-value-of var))))
    (is (= 3 ($ var)))
    (is (= 3 (raw-value-of var)))))

(def-test $ (:compile-at :definition-time)
  ($-test))

(defun $-slot-test ()
  (let ((var (tvar))
        (log (make-tlog)))

    (is (eq +unbound-tvar+ ($ var)))
    (signals unbound-slot ($-slot var))
    (is-false (bound-$? var))
    (is (eq +unbound-tvar+ (raw-value-of var)))

    (setf ($-slot var) 0)
    (is (= 0 ($ var)))
    (is (= 0 ($-slot var)))
    (is-true (bound-$? var))
    (is (= 0 (raw-value-of var)))

    (with-recording-to-tlog log
      ;; global-clock GV5 used by hardware transactions
      ;; causes 50% of software transactions to abort... work around it
      (setf (stmx::tlog-read-version log) (stmx::tvar-version var))

      (is (= 0 ($ var)))
      (is (= 0 ($-slot var)))
      (is-true (bound-$? var))
      (is (= 0 (raw-value-of var)))

      (unbind-$ var)
      (is (eq +unbound-tvar+ ($ var)))
      (is-false (bound-$? var))
      (signals unbound-slot ($-slot var))
      (is (= 0 (raw-value-of var)))

      (is-true (valid? log))
      (is-true (commit log))
      (is (eq +unbound-tvar+ (raw-value-of var))))

    (is (eq +unbound-tvar+ ($ var)))
    (signals unbound-slot ($-slot var))
    (is-false (bound-$? var))
    (is (eq +unbound-tvar+ (raw-value-of var)))))
    
(def-test $-slot (:compile-at :definition-time)
  ($-slot-test))

      



(defun atomic-test ()
  (let ((var (tvar 0))
        (counter 0))

    (atomic
      (is (= 1 (incf counter)))
      (is (= 0 (raw-value-of var)))
      (is (= 0 ($ var)))
      (setf ($ var) 1)
      (is (= 0 (raw-value-of var)))
      (is-true (valid? (current-tlog))))

    (is (= 1 ($ var)))))

(def-test atomic (:compile-at :definition-time)
  (atomic-test))



(define-condition test-error (simple-error)
  ())

(def-test rollback (:compile-at :definition-time)
  (let1 var (tvar 1)
    
    (signals test-error
      (atomic
       (setf ($ var) 2)
       (error 'test-error :format-arguments "test-error signalled to cause rollback"))
      
      (fail "error signaled inside ATOMIC was not propagated to caller"))

    (is (= (raw-value-of var) 1))
    (is (= ($ var) 1))))


;; test (return-from function-name) used inside (transaction (defun ...))

(transaction
 (defun transaction-return-from (var value &key return-from?)
   (declare (type tvar var)
            (type fixnum value)
            (type boolean return-from?))
   (when return-from?
     (return-from transaction-return-from (setf ($ var) value)))
   (setf ($ var) (- value))))


(transaction
 (defun (setf transaction-return-from) (value var &key return-from?)
   (declare (type tvar var)
            (type fixnum value)
            (type boolean return-from?))
   (when return-from?
     (return-from transaction-return-from (setf ($ var) value)))
   (setf ($ var) (- value))))

     
(def-test transaction-return-from (:compile-at :definition-time)
  (let1 var (tvar 1)
    
    (transaction-return-from var 2 :return-from? nil)
    (is (= -2 ($ var)))

    (transaction-return-from var 3 :return-from? t)
    (is (= 3 ($ var)))

    (setf (transaction-return-from var :return-from? nil) 4)
    (is (= -4 ($ var)))

    (setf (transaction-return-from var :return-from? t) 5)
    (is (= 5 ($ var)))))

    

    
  
