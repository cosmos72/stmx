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

