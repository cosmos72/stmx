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


(in-package :cl-user)

(asdf:defsystem :stmx
  :name "STMX"
  :version "2.0.5"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "Composable Transactional Memory"

  :depends-on (:alexandria
               :log4cl
               :closer-mop
               :bordeaux-threads
               :trivial-garbage)

  :components
  ((:static-file "stmx.asd")
   
   (:module :asm
    :components #+(and sbcl (or x86 x86-64))
                ((:file "package")
                 (:file "compiler"        :depends-on ("package"))
                 (:file "x86-32,64-known" :depends-on ("compiler"))
                 (:file "x86-32,64-insts" :depends-on ("x86-32,64-known"))
                 (:file "x86-32,64-vops"  :depends-on ("x86-32,64-insts"))
                 (:file "cpuid"           :depends-on ("x86-32,64-vops"))
                 (:file "transaction"     :depends-on ("x86-32,64-vops")))

                #+(and sbcl (not (or x86 x86-64)))
                ((:file "package")
                 (:file "compiler"        :depends-on ("package"))
                 (:file "notransaction"   :depends-on ("compiler")))

                #-sbcl
                ())

   (:module :lang
    :components ((:file "package")
                 (:file "macro"           :depends-on ("package"))
                 (:file "features"        :depends-on ("macro"))
                 (:file "features-reader" :depends-on ("features"))
                 (:file "thread"          :depends-on ("features-reader"))
                 (:file "features-detect" :depends-on ("thread"))
                 (:file "hw-transactions" :depends-on ("features-detect"))
                 (:file "atomic-ops"      :depends-on ("features-detect"))
                 (:file "mutex"           :depends-on ("atomic-ops"))
                 (:file "atomic-counter"  :depends-on ("atomic-ops" "mutex"))
                 (:file "cons"            :depends-on ("thread"))
                 (:file "fast-vector"     :depends-on ("macro"))
                 (:file "hash-table"      :depends-on ("cons"))
                 (:file "print"           :depends-on ("macro"))
                 (:file "class-precedence-list" :depends-on ("macro")))
    :depends-on (:asm))

   (:module :main
    :components ((:file "package")
                 (:file "version"        :depends-on ("package"))
                 (:file "global-clock"   :depends-on ("package"))
                 (:file "tvar-fwd"       :depends-on ("global-clock"))
                 (:file "classes"        :depends-on ("tvar-fwd"))
                 (:file "txhash"         :depends-on ("classes"))
                 (:file "tlog"           :depends-on ("txhash"))
                 (:file "tvar"           :depends-on ("tlog"))
                 (:file "optimize-for"   :depends-on ("tvar"))
                 (:file "tvar-slot"      :depends-on ("optimize-for"))
                 (:file "tstruct"        :depends-on ("tvar-slot"))
                 (:file "tclass"         :depends-on ("tvar-slot" "tstruct"))
                 (:file "tslot"          :depends-on ("tclass"))
                 (:file "hw-atomic"      :depends-on ("classes"))
                 (:file "commit"         :depends-on ("tvar" "hw-atomic"))
                 (:file "sw-atomic"      :depends-on ("commit"))
                 (:file "atomic"         :depends-on ("hw-atomic" "sw-atomic"))
                 (:file "orelse"         :depends-on ("atomic")))
    :depends-on (:lang))

   (:module :util
    :components ((:file "package")
                 (:file "misc"           :depends-on ("package"))
                 (:file "print"          :depends-on ("package"))
                 
                 (:file "container"      :depends-on ("misc"))
                 (:file "tcons"          :depends-on ("misc"))
                 (:file "tcons-list"     :depends-on ("tcons"))
                 (:file "tcons-alist"    :depends-on ("tcons"))
                 (:file "tcons-set"      :depends-on ("tcons"))
                 (:file "tcons-tree"     :depends-on ("tcons"))
                 (:file "tcons-higher"   :depends-on ("tcons-alist"))
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
    :depends-on (:lang :main)))

  :in-order-to ((asdf:test-op (asdf:test-op "stmx.test"))))
