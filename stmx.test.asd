(asdf:defsystem :stmx.test
  :name "STMX.TEST"
  :version "2.0.5"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "test suite for STMX"

  :depends-on (:log4cl
               :bordeaux-threads
               :fiveam
               :stmx)

  :components
  ((:module :test
    :components ((:file "package")
                 (:file "hash-table"     :depends-on ("package"))
                 (:file "txhash"         :depends-on ("hash-table"))
                 (:file "ghash-table"    :depends-on ("hash-table"))
                 (:file "thash-table"    :depends-on ("hash-table"))
                 (:file "rbmap"          :depends-on ("hash-table"))
                 (:file "atomic"         :depends-on ("package"))
                 (:file "conflict"       :depends-on ("package"))
                 (:file "on-commit"      :depends-on ("atomic"))
                 (:file "retry"          :depends-on ("package"))
                 (:file "orelse"         :depends-on ("package"))
                 (:file "accessors"      :depends-on ("atomic"))
                 (:file "tmap"           :depends-on ("rbmap" "orelse"))
                 (:file "run-suite"      :depends-on ("tmap")))))

  :perform (asdf:test-op
            (o c)
            (eval (read-from-string "(fiveam:run! 'stmx.test:suite)"))))

