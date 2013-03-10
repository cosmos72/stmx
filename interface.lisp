;; -*- lisp -*-

(in-package :cl-stm2)

(eval-always
  (enable-pf-reader))

;;;; ** Running

(defmacro atomic (&body body)
  (let1 tx (gensym)
    `(let ((,tx (lambda () ,@body)))
       (if (recording?)
           (funcall ,tx)
           (execute ,tx)))))



(defun compute-id-of (tx)
  (let* ((str (format nil "~A" tx))
         (beg (position #\{ str))
         (end (position #\} str)))
    (if (and beg end)
        (subseq str (1+ beg) end)
        str)))

(let1 ids (make-hash-table :test 'eq :size 100 :weakness :key)
  (defun id-of (tx)
    (or
     (gethash tx ids)
     (setf (gethash tx ids) (compute-id-of tx)))))

(defun run-once (tx)
  (let1 id (id-of tx)
    (with-new-tlog log
      (stm.run-once.dribble "Tlog ~A created" (tlog-id log))
      (stm.run-once.dribble "Transaction ~A starting..." id)
      (let1 x (catch 'retry (multiple-value-list (funcall tx)))
        (etypecase x
          (tlog
           (stm.run-once.debug "Transaction ~A retried" id)
           (values t x))
          (list
           (stm.run-once.debug "Transaction ~A completed, return values: ~{~A ~}" id x)
           (values nil log x)))))))


(defun execute (tx)
  (let1 id (id-of tx)
    (prog (x-tlog x-values)
     execute
     (multiple-value-bind (retry? log values) (run-once tx)
       (if retry?
           (progn
             (stm.execute.dribble "Transaction ~A will be re-executed" id)
             (wait-tlog log)
             (go execute))
           (progn
             (setq x-tlog   log
                   x-values values)
             (go commit))))
     commit
     (if (not (check? x-tlog))
         (progn
           (stm.execute.dribble "Transaction ~A will be re-executed immediately" id)
           (go execute))
         (if (not (commit x-tlog))
             (progn
               (stm.execute.dribble "Transaction ~A will be re-committed" id)
               (wait-tlog x-tlog)
               (go commit))
             (go done)))
     done
     (return-from execute (values-list x-values)))))




;;;; ** Composing

(defun execute-orelse (tx1 tx2)
  (let ((id1 (id-of tx1))
        (id2 (id-of tx2)))
    (prog (log1 log2 x-values)
     execute-tx1
     (multiple-value-bind (retry? log values) (run-once tx1)
       (if retry?
           (progn
             (stm.orelse.dribble "Transaction ~A retried, trying transaction ~A" id1 id2)
             (go execute-tx2))
           (progn
             (setq log1     log
                   x-values values)
             (go commit-tx1))))
     commit-tx1
     (if (not (check? log1))
         (progn
           (stm.orelse.dribble "Tlog ~A of transaction ~A invalid, trying transaction ~A"
                               (tlog-id log1) id1 id2)
           (go execute-tx2))
         (if (not (commit log1))
             (progn
               (stm.orelse.dribble "Tlog ~A of transaction ~A not committed, trying transaction ~A"
                                   (tlog-id log1) id1 id2)
               (go execute-tx2))
             (go done)))
     execute-tx2
     (multiple-value-bind (retry? log values) (run-once tx2)
       (if retry?
           (progn
             (stm.orelse.dribble "Transaction ~A retried, retrying both ~A and ~A"
                                 id2 id1 id2)
             (throw 'retry (merge-tlogs log1 log)))
           (progn
             (setq log2     log
                   x-values values)
             (go commit-tx2))))
     commit-tx2
     (if (not (check? log2))
         (progn
           (stm.orelse.dribble "Tlog ~A of transaction ~A invalid, retrying both ~A and ~A"
                               (tlog-id log2) id2 id1 id2)
           (throw 'retry (merge-tlogs log1 log2)))
         (if (not (commit log2))
             (progn
               (stm.orelse.dribble "Tlog ~A of transaction  ~A not committed, retrying both ~A and ~A"
                                   (tlog-id log2) id2 id1 id2)
               (throw 'retry (merge-tlogs log1 log2)))
             (go done)))
     done
     (return-from execute-orelse (values-list x-values)))))


(defmacro orelse (form1 form2)
  `(execute-orelse (lambda () ,form1)
                   (lambda () ,form2)))
  

(defmacro try (&body body)
  "Return a transaction that executes each transaction in BODY
atomically from left to right until one succeeds.

The return value of the transaction is the value of the
transaction that succeeds."
  (reduce [list 'orelse] body :from-end t))


;;;; ** Retrying

(defun retry ()
  "Abort the current transaction and re-executes it from scratch.

The transaction will wait on all variables that have been read so
far during the transaction."
  (throw 'retry (current-tlog)))

  

;; Copyright (c) 2006 Hoan Ton-That
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
