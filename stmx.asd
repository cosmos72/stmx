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


(asdf:defsystem :stmx
  :name "STMX"
  :version "0.5"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "Composable Software Transactional Memory"

  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop
               :log4cl)

  :components ((:static-file "stmx.asd")

               (:module :src
                :components ((:file "package")
                             (:file "misc"      :depends-on ("package"))
                             (:file "classes"   :depends-on ("misc"))
                             (:file "tlog"      :depends-on ("classes"))
                             (:file "tvar"      :depends-on ("tlog"))
                             (:file "tclass"    :depends-on ("tvar"))
                             (:file "atomic"    :depends-on ("tclass"))))

               (:module :util
                :components ((:file "package")
                             (:file "cell-obj"  :depends-on ("package"))
                             (:file "cell-tvar" :depends-on ("package")))
                :depends-on (:src))))



(asdf:defsystem :stmx.test
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "test suite for STMX"

  :depends-on (:arnesi
               :bordeaux-threads
               :log4cl
               :fiveam
               :stmx)

  :components ((:module :test
                :components ((:file "package")
                             (:file "suite"     :depends-on ("package"))))))


