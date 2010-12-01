(asdf:defsystem :eager-future2
  :name "eager-future2"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :license "LLGPLv3"
  :serial t
  :components ((:file "package")
               (:file "scheduler")
               (:file "make-future")
               (:file "future")
               (:file "library"))
  :depends-on (:bordeaux-threads :trivial-garbage))
