(asdf:defsystem :eager-future
  :name "eager-future2"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :license "LLGPLv3"
  :serial t
  :components ((:file "package")
               (:file "scheduler")
               (:file "future")
               )
  :depends-on (:bordeaux-threads :trivial-garbage))
