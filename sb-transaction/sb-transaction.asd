;; -*- lisp -*-

;; This file is part of SB-TRANSACTION.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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
  :version "1.3.2"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "Composable Software Transactional Memory"

  :depends-on (:log4cl
               :closer-mop
               :bordeaux-threads
               :trivial-garbage)

  :components ((:static-file "stmx.asd")

               
               (:module :sb-transaction
                :components #-(and sbcl x86-64)
                            ()
                            #+(and sbcl x86-64)
                            ((:file "package")
                             (:file "compiler"     :depends-on ("package"))
                             (:file "x86-64-insts" :depends-on ("compiler"))
                             (:file "x86-64-vm"    :depends-on ("x86-64-insts"))
                             (:file "cpuid"        :depends-on ("x86-64-vm"))
                             (:file "transaction"  :depends-on ("x86-64-vm"))))

               (:module :lang
                :components ((:file "package")
                             (:file "macro"          :depends-on ("package"))
                             (:file "features"       :depends-on ("macro"))
                             (:file "reader"         :depends-on ("features"))
                             (:file "thread"         :depends-on ("features"))
                             (:file "atomic-ops"     :depends-on ("features"))
                             (:file "mutex"          :depends-on ("atomic-ops"))
                             (:file "atomic-counter" :depends-on ("atomic-ops" "mutex"))
                             (:file "cons"           :depends-on ("thread"))
                             (:file "fast-vector"    :depends-on ("macro"))
                             (:file "hash-table"     :depends-on ("cons"))
                             (:file "print"          :depends-on ("macro"))
                             (:file "class-precedence-list" :depends-on ("macro")))
                :depends-on (:sb-transaction))

               (:module :src
                :components ((:file "package")
                             (:file "classes"        :depends-on ("package"))
                             (:file "txhash"         :depends-on ("classes"))
                             (:file "tlog"           :depends-on ("txhash"))
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

                             (:file "gmap"           :depends-on ("misc" "print"))
                             (:file "rbmap"          :depends-on ("gmap"))
                             (:file "tmap"           :depends-on ("rbmap"))

                             (:file "simple-tvector" :depends-on ("print"))

                             (:file "ghash-table"    :depends-on ("print"))
                             (:file "thash-table"    :depends-on ("ghash-table" "simple-tvector")))
                :depends-on (:lang :src))))



(asdf:defsystem :stmx.test
  :name "STMX.TEST"
  :version "1.3.2"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "test suite for STMX"

  :depends-on (:log4cl
               :bordeaux-threads
               :fiveam
               :stmx)

  :components ((:module :test
                :components ((:file "package")
                             (:file "misc"           :depends-on ("package"))
			     (:file "hash-table"     :depends-on ("misc"))
                             (:file "txhash"         :depends-on ("hash-table"))
                             (:file "ghash-table"    :depends-on ("hash-table"))
                             (:file "thash-table"    :depends-on ("hash-table"))
                             (:file "rbmap"          :depends-on ("hash-table"))
                             (:file "atomic"         :depends-on ("package"))
                             (:file "on-commit"      :depends-on ("atomic"))
                             (:file "retry"          :depends-on ("package"))
                             (:file "orelse"         :depends-on ("package"))
                             (:file "tmap"           :depends-on ("rbmap" "orelse"))))))


(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :stmx))))
  (asdf:load-system :stmx.test)
  (eval (read-from-string "(fiveam:run! 'stmx.test:suite)")))

