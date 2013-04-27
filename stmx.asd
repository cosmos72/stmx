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


(in-package :cl-user)

(asdf:defsystem :stmx
  :name "STMX"
  :version "1.0.3"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "Composable Software Transactional Memory"

  :depends-on (:log4cl
               :closer-mop
               :bordeaux-threads
               :trivial-garbage)

  :components ((:static-file "stmx.asd")

               (:module :lang
                :components ((:file "package")
                             (:file "macro"          :depends-on ("package"))
                             (:file "features"       :depends-on ("macro"))
                             (:file "thread"         :depends-on ("features"))
                             (:file "atomic-counter" :depends-on ("features"))
                             (:file "slow-lock-rw"   :depends-on ("features"))
                             (:file "fast-lock-rw"   :depends-on ("features"))
                             (:file "fast-vector"    :depends-on ("macro"))
                             (:file "hash-table"     :depends-on ("macro"))
                             (:file "print"          :depends-on ("macro"))
                             (:file "class-precedence-list" :depends-on ("macro"))))

               (:module :src
                :components ((:file "package")
                             (:file "classes"        :depends-on ("package"))
                             (:file "tlog"           :depends-on ("classes"))
                             (:file "tvar"           :depends-on ("tlog"))
                             (:file "tclass"         :depends-on ("tvar"))
                             (:file "commit"         :depends-on ("tlog"))
                             (:file "atomic"         :depends-on ("tclass" "commit"))
                             (:file "orelse"         :depends-on ("atomic")))
                :depends-on (:lang))


               (:module :util
                :components ((:file "package")
                             (:file "misc"           :depends-on ("package"))
                             (:file "print"          :depends-on ("package"))

                             (:file "container"      :depends-on ("misc"))
                             (:file "tcons"          :depends-on ("misc"))
                             (:file "tvar"           :depends-on ("container"))
                             (:file "tcell"          :depends-on ("container"))
                             (:file "tstack"         :depends-on ("container"))
                             (:file "tfifo"          :depends-on ("container" "tcons"))
                             (:file "tchannel"       :depends-on ("container" "tcons"))

			     (:file "bheap"          :depends-on ("container"))

                             (:file "bmap"           :depends-on ("misc" "print"))
                             (:file "rbmap"          :depends-on ("bmap"))
                             (:file "tmap"           :depends-on ("rbmap"))

                             (:file "thash-table"    :depends-on ("print")))
                :depends-on (:lang :src))))



(asdf:defsystem :stmx.test
  :name "STMX.TEST"
  :version "1.0.1"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "test suite for STMX"

  :depends-on (:log4cl
               :bordeaux-threads
               :fiveam
               :stmx)

  :components ((:module :test
                :components ((:file "package")
                             (:file "misc"         :depends-on ("package"))
                             (:file "rbmap"        :depends-on ("misc"))
                             (:file "atomic"       :depends-on ("package"))
                             (:file "on-commit"    :depends-on ("atomic"))
                             (:file "retry"        :depends-on ("package"))
                             (:file "orelse"       :depends-on ("package"))
                             (:file "tmap"         :depends-on ("rbmap" "orelse"))))))


(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :stmx))))
  (asdf:load-system :stmx.test)
  (eval (read-from-string "(fiveam:run! 'stmx.test:suite)")))


