;;;; stmx.asd

(asdf:defsystem :stmx
  :author "Massimiliano Ghilardi, Hoan Ton-That"
; :serial t
  :description "Software Transactional Memory"
  :license "LGPL"

  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop
               :log4cl)

  :components ((:static-file "stmx.asd")
               (:file "package")
               (:file "misc"      :depends-on ("package"))
               (:file "classes"   :depends-on ("misc"))
               (:file "tlog"      :depends-on ("classes"))
               (:file "tvar"      :depends-on ("tlog"))
               (:file "tclass"    :depends-on ("tvar"))
               (:file "atomic"    :depends-on ("tclass"))

               (:module :util
                :components ((:file "package")
                             (:file "cell-obj"   :depends-on ("package"))
                             (:file "cell-tvar"  :depends-on ("package"))
#|
                             (:file "chan")
                             (:file "counter")
                             (:file "queue")
                             (:file "utils")
|#
                             )
                :depends-on ("atomic"))))


(asdf:defsystem :stmx.test
  :author "Massimiliano Ghilardi, Hoan Ton-That"
  :license "LGPL"

  :depends-on (:arnesi
               :bordeaux-threads
               :log4cl
               :fiveam
               :stmx)

  :components ((:module :test
                :components ((:file "package")
                             (:file "tlog"     :depends-on ("package")))))

  :in-order-to ((compile-op (load-op :stmx))))



;; Copyright (c) 2013, Massimiliano Ghilardi
;; This file is part of STMX.
;;
;; STMX is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; STMX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with STMX. If not, see <http://www.gnu.org/licenses/>.



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
