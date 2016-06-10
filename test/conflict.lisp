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

(def-suite conflict-suite :in suite)
(in-suite conflict-suite)

(defun conflict-test ()
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

(def-test conflict (:compile-at :definition-time)
  (conflict-test))

(defun conflict-test-1 ()
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

(def-test conflict-1 (:compile-at :definition-time)
  (conflict-test-1))

(defun conflict-test-2 ()
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


(def-test conflict-2 (:compile-at :definition-time)
  (conflict-test-2))

(defstruct ipc
  (command  t   :type (or (member t nil) function))
  (args     nil :type list)
  (ret-list t   :type (or (member t) list))
  (lock     (bt:make-lock))
  (wait-cmd (bt:make-condition-variable))
  (wait-ret (bt:make-condition-variable)))


(defun ipc-run (ipc)
  (declare (type ipc ipc))

  (let ((lock     (ipc-lock     ipc))
        (wait-cmd (ipc-wait-cmd ipc))
        (wait-ret (ipc-wait-ret ipc)))
    (loop
       (with-lock (lock)
         (log:trace "ipc thread ready")

         (loop while (eq t (ipc-command ipc))
            do (bt:condition-wait wait-cmd lock #-ecl :timeout #-ecl 1))

         (let ((command (ipc-command ipc))
               (args    (ipc-args    ipc)))

           (setf (ipc-command  ipc) t)

           (log:trace "running command ~A ~S" command args)
           (let ((ret-list
                  (when command
                    (multiple-value-list (apply command args)))))
             (cond
               ((null ret-list)        (log:trace "command returned no values"))
               ((null (rest ret-list)) (log:trace "command returned: ~S" (first ret-list)))
               (t                      (log:trace "command returned values: ~{~S~^ ~}" ret-list)))
                
             (setf (ipc-ret-list ipc) ret-list))

           (bt:condition-notify wait-ret)

           (unless command
             (return))))))

  (log:trace "ipc thread exiting"))



(defun ipc-call (ipc command &rest args)
  (declare (type ipc ipc)
           (type (or null function) command))

  (let ((lock     (ipc-lock     ipc))
        (wait-cmd (ipc-wait-cmd ipc))
        (wait-ret (ipc-wait-ret ipc)))
    (with-lock (lock)
      (setf (ipc-command  ipc) command
            (ipc-args     ipc) args
            (ipc-ret-list ipc) t)
      
      (bt:condition-notify wait-cmd)

      (loop for ret-list = (ipc-ret-list ipc)
         while (eq t ret-list)
         do (bt:condition-wait wait-ret lock)
         finally (return (values-list ret-list))))))

        
(defun ipc-start-thread (ipc)
  (start-thread (lambda () (ipc-run ipc))))

(defun ipc-stop-thread (ipc)
  (ipc-call ipc nil))


            

(defun conflict-locked-test ()
  (start-multithreading)

  (let ((a (tvar 0)) ;; transactions in this example maintain
        (b (tvar 0)) ;; the invariant (= ($ a) ($ b))
	(ipc (make-ipc))
        (first-run t))

    ;; have another thread locking b:
    ;; the current transaction will fail to commit,
    ;; and further read/writes on b will (rerun)
    ;;
    ;; we *really* need to lock b in another thread,
    ;; otherwise on some implementations (valid-and-unlocked?)
    ;; may not detect the conflict...
    (ipc-start-thread ipc)

    (atomic
     (unless first-run
       ;; on second run, tell the other thread to unlock B before proceeding
       (is-false (ipc-call ipc #'unlock-tvar b))
       (is-true (try-lock-tvar b))
       (unlock-tvar b))
	    

     (setf ($ a) ($ b))
     (incf ($ a))
     (is-true (valid? (current-tlog)))

     ;; on first run, tell the other thread to lock B before proceeding
     (when first-run
       (setf first-run nil)
       (is-true (ipc-call ipc #'try-lock-tvar b))

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
    (is (= 1 ($ b)))

    (ipc-stop-thread ipc)))



;; CMUCL currently does not have native threads
#-cmucl
(def-test conflict-multithread (:compile-at :definition-time)
  #?+bt/make-thread
  (conflict-locked-test))
