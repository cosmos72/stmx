;;;; stmx.asd

(asdf:defsystem #:stmx
  :author "Massimiliano Ghilardi, Hoan Ton-That <hoan@ton-that.org>"
; :serial t
  :description "Software Transactional Memory"
  :license "BSD 3-Clause License"

  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop
               :log4cl)

  :components ((:static-file "cl-stm2.asd")
               (:file "package")
               (:file "utils"     :depends-on ("package"))
               (:file "vbox"      :depends-on ("package"))
               (:file "protocol"  :depends-on ("package" "vbox"))
               (:file "tlog"      :depends-on ("protocol"))
               (:file "tvar"      :depends-on ("vbox" "tlog"))
               (:file "tclass"    :depends-on ("tlog" "tvar"))
               (:file "atomic"    :depends-on ("tlog" "tvar"))))

