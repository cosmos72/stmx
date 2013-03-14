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
	(v1  (new 'tvar :value 1)))
    (is-true (valid? log))
    (read-tvar v1 log)
    (is-true (valid? log))
    (setf (raw-value-of v1) 2)
    (is-false (valid? log))
    (read-tvar v1 log)
    (is-false (valid? log))
    (setf (raw-value-of v1) 1)
    (is-true (valid? log)))))
    
(test commit
  (let ((log (new 'tlog))
	(v1  (new 'tvar :value 1)))
    (write-tvar v1 2 log)
    (commit log)
    (is-true (= (raw-value-of v1) 2))))


(test $
  (let1 v1  (new 'tvar :value 1)
    (is-true (= ($ v1) 1))
    (with-new-tlog log
      (with-recording
        (is-true (= ($ v1) 1))
        (setf ($ v1) 2)
        (is-true (= ($ v1) 2))
        (is-true (= (raw-value-of v1) 1))
        (is-true (valid? log))
        (commit log)
        (is-true (= (raw-value-of v1) 2))))
    (is-true (= ($ v1) 2))))

(test atomic
  (let1 v1  (new 'tvar :value 1)
    (is-true (= ($ v1) 1))
    (atomic :id 'test-atomic
      (is-true (= ($ v1) 1))
      (setf ($ v1) 2)
      (is-true (= ($ v1) 2))
      (is-true (= (raw-value-of v1) 1))
      (is-true (valid? (current-tlog))))
    (is-true (= ($ v1) 1))))
  
    

(test notify-tvar
  (without-returning
    (let1 c (new 'counter :count 1)
      (is-true (= (value-of (count-of c)) 1))
      (perform (increment c))
      (is-true (= (value-of (count-of c)) 2))
      (acquire-lock (lock-of (count-of c)))
      (perform (increment c))
      (is-true (= (value-of (count-of c)) 2))
      (release-lock (lock-of (count-of c)))
      (unwait (count-of c))
      (is-true (= (value-of (count-of c)) 3)))))

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
