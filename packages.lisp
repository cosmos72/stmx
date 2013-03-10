;; -*- lisp -*-

;;;; * Welcome to CL-STM2

;;;; CL-STM2 depends on arnesi, bordeaux-threads, and closer-mop.

(in-package :cl-user)

(defpackage :cl-stm2
  (:use :arnesi
        :bordeaux-threads
        :cl
        :cl-user
        :closer-mop)
  (:nicknames :stm2)
  (:shadowing-import-from :arnesi
                          #:else
                          #:specializer ; closer-mop
                          #:until)
  (:shadowing-import-from :closer-mop
                          #:defclass
                          #:standard-class
                          #:defmethod
                          #:standard-generic-function
                          #:ensure-generic-function
                          #:defgeneric
                          #:defclass
                          #:standard-class
                          #:defmethod
                          #:standard-generic-function
                          #:ensure-generic-function
                          #:defgeneric)
  (:export #:try
           #:retry
           #:trans
           #:untrans
           #:deftransaction

           #:transactional-class
           #:transactional-direct-slot
           #:transactional-effective-slot
           #:transactional-object
           #:deftclass

           #:transaction
           #:standard-transaction
           #:perform
           #:execute
           #:orelse
           #:sequence

           #:tlog
           #:commit
           #:check?
           #:merge-tlogs
           #:wait-tlog

           #:tvar
           #:read-tvar
           #:write-tvar
           #:unwait-tvar

           #:recording?
           #:with-recording
           #:without-recording

           #:current-tlog
           #:with-tlog
           #:with-new-tlog

           #:stm
           #:unstm
           #:stms
           #:unstms
           #:with-stm

           #:counter
           #:count-of
           #:increment
           #:decrement
           #:reset
           #:swap

           #:cell
           #:empty?
           #:empty!
           #:take
           #:put
           #:try-put

           #:tqueue
           #:deq
           #:enq
           #:empty?
           #:full?
           #:qlength

           #:chan
           #:port
           #:read-port
           #:write-chan

           #:sleep-after
           #:nob
           #:repeat
           ))

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
