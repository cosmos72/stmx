;;;; stmx.asd

(asdf:defsystem #:stmx
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
               (:file "classes"   :depends-on ("package" "misc"))
               (:file "tlog"      :depends-on ("classes"))
               (:file "tvar"      :depends-on ("tlog"))
               (:file "tclass"    :depends-on ("tlog" "tvar"))
               (:file "atomic"    :depends-on ("tlog" "tvar" "tclass"))

               (:module :util
                :components ((:file "cell-obj")
                             (:file "cell-tvar")
#|
                             (:file "chan")
                             (:file "counter")
                             (:file "queue")
                             (:file "utils")
|#
                             )
                :depends-on ("atomic"))))



(asdf:defsystem :stmx.test
  :components ((:module :test
                :components ((:file "suite")
                             (:file "tlog" :depends-on ("suite")))))
  :depends-on (:cl-stm :fiveam)
  :in-order-to ((compile-op (load-op :stmx))))
