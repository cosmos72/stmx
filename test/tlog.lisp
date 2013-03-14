;; -*- lisp -*-

(in-package :stmx.test)

(in-suite stmx)

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
    (is-true (valid? log)))))
    
(test commit
  (let ((log (new 'tlog))
	(var (new 'tvar :value 1)))
    (write-tvar var 2 log)
    (is-true (valid? log))
    (commit log)
    (is-true (= (raw-value-of var) 2))))

(test $
  (let1 var (new 'tvar :value 1)
    (is-true (= ($ var) 1))
    (with-new-tlog log
      (with-recording
        (is-true (= ($ var) 1))
        (setf ($ var) 2)
        (is-true (= ($ var) 2))
        (is-true (= (raw-value-of var) 1))
        (is-true (valid? log))
        (commit log)
        (is-true (= (raw-value-of var) 2))))
    (is-true (= ($ var) 2))))

(test atomic
  (let1 var  (new 'tvar :value 1)
    (atomic :id 'test-atomic
      (is-true (= ($ var) 1))
      (setf ($ var) 2)
      (is-true (= ($ var) 2))
      (is-true (= (raw-value-of var) 1))
      (is-true (valid? (current-tlog))))
    (is-true (= ($ var) 2))))

(defun cell-test ()
  (let1 c (new 'cell :value 1)
    (is-false (empty? c))
    (empty! c)
    (is-true (empty? c))
    (put c 2)
    (is-false (empty? c))
    (is-true (= (take c) 2))
    (is-true (empty? c))))

(test cell
  (cell-test))

(test cell-atomic
  (atomic (cell-test)))

#|
(test wait
  (let1 var (new 'tvar :value 1)
    (flet ((f1 ()
             (atomic (incf ($ var)))
             
    
        (t1 (make-thread
    
    (is-true (= ($ var) 2))
    (acquire-lock (lock-of var))
    (unwind-protect
         (progn
           (atomic (incf ($ var)))
           (is-true (= ($ var) 2)))
      (release-lock (lock-of var)))
    (notify-tvar var)
    (is-true (= ($ var) 3))))

(test retry
  (let1 var  (new 'tvar :value 1)
    (atomic :id 'test-retry
      (is-true (= ($ var) 1))
      (setf ($ var) 2)
      (is-true (= ($ var) 2))
      (is-true (= (raw-value-of var) 1))
      (is-false (valid? (current-tlog))))
|#
    


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
