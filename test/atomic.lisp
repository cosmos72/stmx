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
    (setf (tvar-versioned-value var) (cons +invalid-counter+ 1))
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

    (atomic :id 'test-atomic
      (is (= 0 (raw-value-of var)))
      (is (= 0 ($ var)))
      (setf ($ var) 1)
      (is (= 0 (raw-value-of var)))
      (is-true (valid? (current-tlog))))

    (is (= 1 ($ var)))))


(test atomic-invalid-1
  (let ((var (tvar 0)))

    (atomic :id 'test-atomic-invalid-1
      (is (= 0 (raw-value-of var)))
      (is (= 0 ($ var)))
      (setf ($ var) 1)
      (is-true (valid? (current-tlog)))

      ;; simulate another thread committing tvar:
      ;; the current transaction log must become invalid
      (setf (raw-value-of var) -1)
      (is-false (valid? (current-tlog)))
      ;; but reading from tvar must return the value written during transaction
      (is (= 1 ($ var)))
      
      ;; an invalid transaction cannot become valid again
      ;; by writing into its vars. test it.
      (setf ($ var) (raw-value-of var))
      (is-false (valid? (current-tlog)))

      ;; the only way for an invalid transaction to become valid again
      ;; is for some other thread to commit and restore the original value
      ;; and version initially seen by the invalid one.
      ;; Not really possible in the wild because of the version counter.
      (setf (tvar-versioned-value var) (cons +invalid-counter+ 0))
      (is-true (valid? (current-tlog)))
      (setf ($ var) 2))

    (is (= 2 ($ var)))))


(test atomic-invalid-2
  (let ((a (tvar 0)) ;; transactions in this example maintain
        (b (tvar 0)) ;; the invariant (= ($ a) ($ b))
        (first-run t))

    (atomic :id 'test-atomic-invalid-2
      (incf ($ a))
      (is-true (valid? (current-tlog)))

      ;; simulate another thread changing a and b:
      ;; the current transaction will become invalid because it read a
      (when first-run
        (setf first-run nil
              (raw-value-of a) 2
              (raw-value-of b) 2)

        (is-false (valid? (current-tlog)))

        ;; reading from a must return the value written during transaction
        (is (= 1 ($ a)))

        ;; but reading from b must detect the inconsistency and rerun
        (signals rerun-error (incf ($ b))))

      ;; read b again: in first-run it will re-run, in second-run it will succeed
      ;; and restore the invariant (= ($ a) ($ b))
      (incf ($ b)))

    (is (= 3 ($ a)))
    (is (= 3 ($ b)))))



(define-condition test-error (simple-error)
  ())

(test rollback
  (let1 var (tvar 1)
    
    (signals test-error
      (atomic :id 'test-rollback
              (setf ($ var) 2)
              (error 'test-error :format-arguments "test-error signalled to cause rollback"))
      
      (fail "error signaled inside ATOMIC was not propagated to caller"))

    (is (= (raw-value-of var) 1))
    (is (= ($ var) 1))))


(test invalid
  (let ((var (tvar 5))
        (counter 0))
    
    (atomic :id 'test-invalid
      (log:debug "($ var) is ~A" ($ var))
      (incf ($ var))
      (log:debug "($ var) set to ~A" ($ var))

      (if (= 1 (incf counter))
          (progn
            ;; simulate another thread writing into VAR
            (setf (raw-value-of var) 10)
            (log:debug "simulated another thread setting (raw-value-of var) to ~A"
                       (raw-value-of var))
            (is-false (valid? (current-tlog))))
          ;; else
          (is-true (valid? (current-tlog)))))
          
    (is (= 11 ($ var))))) ;; 10 for "(setf (raw-value-of var) 10)" plus 1 for "(incf ($ var))"


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

    
  