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

(test tx-read-of
  (let ((var (make-tvar :value 1))
        (log (make-tlog)))
    (with-recording-to-tlog log
      (is (= 1 (raw-value-of var)))
      (is (= 1 (tx-read-of var)))
      (tx-write-of var 2)
      (is (= 1 (raw-value-of var)))
      (is (= 2 (tx-read-of var))))))

(test valid?
  (let ((var (make-tvar :value 1))
        (log (make-tlog)))
    (with-recording-to-tlog log
      (is-true (valid? log))
      (tx-read-of var log)
      (is-true (valid? log))
      (setf (raw-value-of var) 2)
      (is-false (valid? log))
      (tx-read-of var log)
      (is-false (valid? log))
      (setf (raw-value-of var) 1)
      (is-true (valid? log)))))
    
(test commit
  (let ((var (make-tvar :value 1))
        (log (make-tlog)))
    (with-recording-to-tlog log
      (tx-write-of var 2 log)
      (is-true (valid? log))
      (is-true (commit log))
      (is (= 2 (raw-value-of var))))))

(test $
  (let ((log (make-tlog))
        (var (make-tvar :value 1)))
    (is (= 1 ($ var)))
    (with-recording-to-tlog log
      (is (= 1 ($ var)))
      (setf ($ var) 2)
      (is (= 2 ($ var)))
      (is (= 1 (raw-value-of var)))
      (is-true (valid? log))
      (is-true (commit log))
      (is (= 2 (raw-value-of var))))
    (is (= 2 ($ var)))))

(test atomic
  (let1 var (make-tvar :value 0)

    (atomic :id 'test-atomic
      (is (= 0 ($ var)0))
      (setf ($ var) 1)
      (is (= 0 (raw-value-of var)))
      (is-true (valid? (current-tlog)))

      ;; simulate another thread committing tvar:
      ;; the current transaction log must become invalid
      (setf (raw-value-of var) -1)
      (is (= 1 ($ var)))
      (is-false (valid? (current-tlog)))
      
      ;; an invalid transaction cannot become valid again
      ;; by writing into its vars. test it.
      (setf ($ var) (raw-value-of var))
      (is-false (valid? (current-tlog)))

      ;; the only way for an invalid transaction to become valid again
      ;; is for some other thread to commit and "by chance"
      ;; restore the original value initially seen by the invalid one
      (setf (raw-value-of var) 0)
      (is-true (valid? (current-tlog)))
      (setf ($ var) 2))

    (is (= 2 ($ var)))))

(define-condition test-error (simple-error)
  ())

(test rollback
  (let1 var (make-tvar :value 1)
    
    (signals test-error
      (atomic :id 'test-rollback
              (setf ($ var) 2)
              (error 'test-error :format-arguments "test-error signalled to cause rollback"))
      
      (fail "error signaled inside ATOMIC was not propagated to caller"))

    (is (= (raw-value-of var) 1))
    (is (= ($ var) 1))))


(test invalid
  (let ((var (make-tvar :value 1))
        (counter 0)
        masked-test-error?)
    
    (handler-case
        (progn
          (atomic :id 'test-invalid
                  (log:debug "($ var) is ~A" ($ var))
                  (incf ($ var))
                  (log:debug "($ var) set to ~A" ($ var))
                  (when (= 1 (incf counter))
                    ;; simulate another thread writing into VAR
                    (setf (raw-value-of var) 10)
                    (log:debug "simulated another thread setting (raw-value-of var) to ~A"
                               (raw-value-of var))
                    (is-false (valid? (current-tlog)))
                    (error 'test-error :format-arguments
                           "BUG! an error signalled from an invalid transaction
was propagated outside (atomic)"))

                  (is-true (valid? (current-tlog))))
          
          ;; the test error we signalled from an invalid transaction
          ;; must not propagate outside (atomic), so this code must execute
          (setf masked-test-error? t))

      (test-error (err)
        (fail "~A ~A" (type-of err) err)))

    (is-true masked-test-error?)
    (is (= 11 ($ var))))) ;; 10 for "(setf (raw-value-of var) 10)" plus 1 for "(incf ($ var))"


