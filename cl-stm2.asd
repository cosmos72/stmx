;;;; cl-stm2.asd

(asdf:defsystem #:cl-stm2
  :author "Hoan Ton-That <hoan@ton-that.org>, Massimiliano Ghilardi"
;  :serial t
  :description "Software Transactional Memory"
  :license "BSD 3-Clause License"

  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop)

  :components ((:static-file "cl-stm2.asd")
               (:file "packages")
               (:file "loggers" :depends-on ("packages"))
               (:file "protocol" :depends-on ("packages"))
               (:file "tlog" :depends-on ("protocol" "loggers"))
               (:file "tvar" :depends-on ("tlog"))

#|
               (:file "standard-transaction" :depends-on ("tlog"))
               (:file "transactional-class" :depends-on ("tlog"))
               (:file "walker" :depends-on ("protocol"))
               (:file "interface" :depends-on ("transactional-class" "standard-transaction" "standard-tlog" "walker"))
|#
               ))

