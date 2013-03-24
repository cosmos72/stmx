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


(in-package :stmx)

;;;; ** Composing

(define-condition orelse-error (control-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Attempt to use ~A or ~A outside an ~A block" 'try 'orelse 'atomic))))


(defun run-orelse2 (tx1 tx2 &key id1 id2)
  "Function variant of `orelse2'. Execute function TX1 first,
then function TX2 in separate, nested transactions until one succeeds
\(i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  (declare (type function tx1 tx2))

  ;; binary implementation, two transactions tx1 and tx2: run tx1, then:
  ;;
  ;; a) if tx1 is executed:
  ;;    1. if tx1 is invalid, run tx2. reason: more possibilities to get a valid run of tx1 or tx2
  ;;    2. if tx1 wants to retry, run tx2. reason: orelse exists exactly to implement this behaviour
  ;;    3. if tx1 wants to commit (returns normally), merge to parent log and return
  ;;    4. if tx1 wants to rollback (signals an error), rollback and signal the error
  ;; 
  ;; b) if tx2 is executed:
  ;;    1. if tx2 is invalid:
  ;;       1.1 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;           this is necessary to break infinite loops alternating
  ;;           between tx1 and tx2, caused by the parent-tx being invalid
  ;;       1.2 if tx1 is invalid, run tx1. reason: it means some TVARs
  ;;           relevant for tx1 have changed
  ;;       1.3 run tx2 again
  ;;    2. if tx2 wants to retry, sleep on tx1+tx2
  ;;    3. if tx2 wants to commit (returns normally), merge to parent log and return
  ;;    4. if tx2 wants to rollback (signals an error), rollback and signal the error
  ;; 
  ;; c) if sleeping on tx1+tx2:
  ;;    1. if tx1 and tx2 are incompatible => at least one of them is invalid
  ;;       1.1 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       1.2 otherwise, run the first invalid one between tx1 and tx2
  ;;    2. merge tx1 and tx2 logs into log1+2, and sleep on it. when waking up:
  ;;       2.1 if log1+2 is valid, sleep again on it
  ;;       2.2 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       2.3 run tx1 (we could check which tx is invalid to run it instead,
  ;;           but we lost tx1 log when merging tx1 and tx2)



  ;; n-ary implementation, n transactions tx[0...n-1]: set i=0, run tx[i], then:
  ;;
  ;; a) if tx[i] is executed:
  ;;    1. if tx[i] is invalid:
  ;;       1.1 if tx[i+1] exists, run it. reason: more possibilities
  ;;           to get a valid run of one of tx[]
  ;;       1.2 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;           this is necessary to break infinite loops cycling
  ;;           on some of the tx[i], caused by the parent-tx being invalid
  ;;       1.3 if i<>0 and tx[0] is invalid, run it. reason: it means some TVARs
  ;;           relevant for tx[0] have changed, and there is no reason
  ;;           to re-execute parent-tx
  ;;       1.4 run tx[i] again
  ;;    2. if tx[i] wants to retry:
  ;;       2.1. if tx[i+1] exists, run it. reason: orelse exists exactly
  ;;            to implement this behaviour
  ;;       2.2. otherwise, sleep on tx[0]+ ... + tx[n-1]
  ;;    3. if tx[i] wants to commit (returns normally), merge to parent log and return
  ;;    4. if tx[i] wants to rollback (signals an error), rollback and signal the error
  ;; 
  ;; c) if sleeping on tx[0]+ ... + tx[n-1]:
  ;;    1. if tx[0] ... tx[n-1] are incompatible => at least one of them is invalid
  ;;       1.1 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       1.2 otherwise, run the first invalid one among tx[0] ... tx[n-1]
  ;;    2. merge tx[0] ... tx[n-1] logs into logN, and sleep on it. when waking up:
  ;;       2.1 if logN is valid, sleep again on it
  ;;       2.2 if parent-tx is invalid, tell it to re-execute with (rerun)
  ;;       2.3 run tx[0] (we could check which tx is invalid to run it instead,
  ;;           but we lost some logs when merging them)


  (let1 parent-log (current-tlog)
    (unless parent-log
      (error 'orelse-error))

    (when id1
      (setf (~ tx1) id1))
    (when id2
      (setf (~ tx2) id2))

    (prog (log1 log2 x-values)

     execute-tx1
     (setf log1 (make-or-clear-tlog log1 :parent parent-log))

     (handler-bind ((condition
                     (lambda (err)
                       (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                                  (~ log1) (~ tx1) (type-of err) (~ err))
                       (if (eq t (locked-valid? log1))
                           ;; return normally from lambda to propagate the error
                           (log:debug "Tlog ~A {~A} will rollback and signal ~A"
                                      (~ log1) (~ tx1) (type-of err))
                           (progn
                             (log:debug "Tlog ~A {~A} is invalid or unknown, masking the ~A it signalled and trying ~A"
                                        (~ log1) (~ tx1) (type-of err) (~ tx2))
                             (go execute-tx2))))))
       
       (multiple-value-bind (retry? values) (run-once tx1 log1)
         (when retry?
           (log:debug "Tlog ~A {~A} wants to retry, trying {~A}"
                      (~ log1) (~ tx1) (~ tx2))
           (go execute-tx2))
	 
         (when (invalid? log1)
           (log:debug "Tlog ~A {~A} is invalid, trying {~A}"
                      (~ log1) (~ tx1) (~ tx2))
           (go execute-tx2))
	 
         (setf x-values values)
         (go commit-tx1)))
   
     commit-tx1
     (commit-nested log1)
     (log:debug "Tlog ~A {~A} merged with parent tlog ~A"
                (~ log1) (~ tx1) (~ parent-log))
     (go done)
   
     execute-tx2
     (setf log2 (make-or-clear-tlog log2 :parent parent-log))

     (handler-bind ((condition
                     (lambda (err)
                       (log:trace "Tlog ~A {~A} wants to rollback, signalled ~A: ~A"
                                  (~ log2) (~ tx2) (type-of err) (~ err))
                       (if (eq t (locked-valid? log1))
                           ;; return normally from lambda to propagate the error
                           (log:debug "Tlog ~A {~A} will rollback and signal ~A"
                                      (~ log2) (~ tx2) (type-of err))
                           (progn
                             (log:debug "Tlog ~A {~A} is invalid or unknown, masking the ~A it signalled and trying ~A"
                                        (~ log2) (~ tx2) (type-of err) (~ tx1))
                             (go execute-tx1))))))

       (multiple-value-bind (retry? values) (run-once tx2 log2)
         (when (invalid? log2)
           (log:debug "Tlog ~A {~A} is invalid, checking what to do..."
                      (~ log2) (~ tx2))
           (when (invalid? parent-log)
             (log:debug "Parent tlog ~A is invalid, re-running the parent transaction"
                        (~ parent-log))
             (rerun))

           (when (invalid? log1)
             (log:debug "Tlog ~A {~A} is invalid, re-running it" (~ log1) (~ tx1))
             (go execute-tx1))

           (log:debug "Tlog ~A {~A} was found invalid, re-running it" (~ log2) (~ tx2))
           (go execute-tx2))

         (when retry?
           (log:trace "Tlog ~A {~A} wants to retry, sleeping then retrying both {~A} and {~A}"
                      (~ log2) (~ tx2) (~ tx1) (~ tx2))
           (go wait-reexecute))

         (setf x-values values)
         (go commit-tx2)))
     
     wait-reexecute
     (unless (compatible-tlogs log1 log2)
       (log:debug "Tlog ~A {~A} and tlog ~A {~A} are incompatible, not going to sleep"
                  (~ log1) (~ tx1) (~ log2) (~ tx2))
       (when (invalid? parent-log)
         (log:debug "Parent tlog ~A is invalid, re-running it"
                    (~ parent-log))
         (rerun))

       (when (invalid? log1)
         (log:debug "Tlog ~A {~A} is invalid, re-running it"
                    (~ log1) (~ tx1))
         (go execute-tx1))

       (progn
         (log:debug "Tlog ~A {~A} deduced to be invalid, re-running it"
                    (~ log2) (~ tx2))
         (go execute-tx2)))

     (log:debug "Sleeping for both tlog ~A {~A} and tlog ~A {~A}"
                (~ log1) (~ tx1) (~ log2) (~ tx2))
     (wait-tlog (merge-reads-of log1 log2))
     (go execute-tx1)


     commit-tx2
     (commit-nested log2)
     (log:debug "Tlog ~A {~A} merged with parent tlog ~A"
                (~ log2) (~ tx2) (~ parent-log))
     (go done)


     done
     (return-from run-orelse2 (values-list x-values)))))


(defmacro orelse2 (form1 form2 &key id1 id2)
  "Execute first FORM1, then FORM2 in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."
  `(run-orelse2 (lambda () ,form1)
                (lambda () ,form2)
                :id1 ,id1
                :id2 ,id2))
  

(defmacro orelse (&body body)
  "Execute each form in BODY from left to right in separate, nested transactions
until one succeeds (i.e. commits) or signals an error.

Returns the value of the transaction that succeeded,
or signals the error raised by the transaction that failed.

Can only be used inside an ATOMIC block."

  (reduce (lambda (x y) (list 'orelse2 x y)) body :from-end t))



(defun run-nonblocking (tx &key id)
  "Function version of NONBLOCKING macro: execute the function tx in a nested transaction and:
a) if the nested transaction succeeds (i.e. commits), return multiple values:
   T followed by the values returned by the transaction.
b) if the nested transaction signals an error, raise such error.
c) if the nested transaction attempts to retry,
   immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (declare (type function tx))
  (orelse2 (multiple-value-call #'values t (funcall tx))
           nil
           :id1 (or id 'nonblocking)
           :id2 'nonblocking-nil))


(defmacro nonblocking (&body body)
  "Execute all the form in BODY in a single nested transaction and:
a) if the nested transaction succeeds (i.e. commits), return multiple values:
   T followed by the values returned by the transaction.
b) if the nested transaction signals an error, raise such error.
c) if the nested transaction attempts to retry,
   immediately return NIL without waiting/sleeping.

Can only be used inside an ATOMIC block."

  (let1 id (when (eq :id (first body))
             (pop body)
             (pop body))
    `(orelse2 (multiple-value-call #'values t (progn ,@body))
              nil
              :id1 ,(or id ''nonblocking)
              :id2 'nonblocking-nil)))
