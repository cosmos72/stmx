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

(def-suite conflict-suite :in suite)
(in-suite conflict-suite)

(test conflict
  (let ((var (tvar 5))
        (counter 0))
    
    (atomic
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


(test conflict-1
  (let ((var (tvar 0)))

    (atomic
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
      (set-tvar-value-and-version var 0 +invalid-version+)

      (is-true (valid? (current-tlog)))
      (setf ($ var) 2))

    (is (= 2 ($ var)))))


(test conflict-2
  (let ((a (tvar 0)) ;; transactions in this example maintain
        (b (tvar 0)) ;; the invariant (= ($ a) ($ b))
        (first-run t))

    (atomic
      (incf ($ a))
      (is-true (valid? (current-tlog)))

      ;; simulate another thread changing a and b:
      ;; the current transaction will become invalid because it read a
      (when first-run
        (setf first-run nil
              (raw-value-of a) 2
              (raw-value-of b) 2)
        (is-false (valid? (current-tlog)))
        ;; reading from a must return the value written during the transaction
        (is (= 1 ($ a)))
        ;; but reading from b must detect the inconsistency and rerun
        (signals rerun-error (incf ($ b))))

      ;; read b: in first-run it will re-run, in second-run it will succeed
      ;; and restore the invariant (= ($ a) ($ b))
      (incf ($ b)))

    (is (= 3 ($ a)))
    (is (= 3 ($ b)))))




(test conflict-locked
  (let ((a (tvar 0)) ;; transactions in this example maintain
        (b (tvar 0)) ;; the invariant (= ($ a) ($ b))
	(wait-to-lock (bt:make-lock))
	(wait-to-unlock (bt:make-lock))
        (first-run t))

    (bt:acquire-lock wait-to-lock)
    (bt:acquire-lock wait-to-unlock)

    ;; have another thread locking b:
    ;; the current transaction will fail to commit,
    ;; and further read/writes on b will (rerun)
    ;;
    ;; we *really* need to lock b in another thread,
    ;; otherwise on some implementations (valid-and-unlocked?)
    ;; may not detect the conflict...
    (bt:make-thread
     (lambda ()
       (log:trace "helper thread ready to lock B")
       (unwind-protect
	    (with-lock (wait-to-lock)
	      (log:trace "locking TVAR B...")
	      (loop until (try-lock-tvar b) do
		   (sleep 0.1))
	      (log:trace "...TVAR B locked"))

	 (log:trace "helper thread ready to unlock B")
	 (with-lock (wait-to-unlock)
	   (log:trace "unlocking TVAR B...")
	   (unlock-tvar b)
	   (log:trace "...TVAR B unlocked")))))


    (atomic
     (unless first-run
       ;; on second run, tell the other thread to unlock B before proceeding
       (bt:release-lock wait-to-unlock)
       ;; wait until actually unlocked
       (loop until (try-lock-tvar b) do
	    (sleep 0.1))
       (unlock-tvar b))
	    

     (setf ($ a) ($ b))
     (incf ($ a))
     (is-true (valid? (current-tlog)))

     (when first-run
       (setf first-run nil)


       ;; on first run, tell the other thread to lock B before proceeding
       (bt:release-lock wait-to-lock)
       ;; wait until actually locked
       (loop while (try-lock-tvar b) do
	    (unlock-tvar b)
	    (sleep 0.1))
       (is-false (try-lock-tvar b))


       ;; transaction is still valid
       (is-true (valid? (current-tlog)))

       ;; but there is a lock in the way
       (is-false (valid-and-unlocked? (current-tlog)))
       
       ;; reading from a must return the value set during the transaction
       (is (= 1 ($ a)))
       ;; but reading from B must detect the lock and rerun
       (signals rerun-error (incf ($ b))))

     ;; this will (rerun) on the first run, and succeed on the second
     (incf ($ b)))

    (is (= 1 ($ a)))
    (is (= 1 ($ b)))))


