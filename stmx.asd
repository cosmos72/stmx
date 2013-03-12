;;;; stmx.asd

(asdf:defsystem #:stmx
  :author "Massimiliano Ghilardi, Hoan Ton-That <hoan@ton-that.org>"
; :serial t
  :description "Software Transactional Memory"
  :license "LGPL"

  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop
               :log4cl)

  :components ((:static-file "stmx.asd")
               (:file "package")
               (:file "utils"     :depends-on ("package"))
               (:file "classes"   :depends-on ("package"))
               (:file "tlog"      :depends-on ("classes"))
               (:file "tvar"      :depends-on ("tlog"))
               (:file "tclass"    :depends-on ("tlog" "tvar"))
               (:file "atomic"    :depends-on ("tlog" "tvar"))))

