(asdf:defsystem :test.eager-future2
  :components ((:module :test
                        :serial t
                        :components ((:file "package")
                                     (:file "test")
                                     (:file "benchmark"))))
  :depends-on (:eager-future2 :eos))
