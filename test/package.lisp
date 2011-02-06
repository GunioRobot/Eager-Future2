(cl:defpackage #:test.eager-future2
  (:export #:run-tests)
  (:use #:cl #:eager-future2 #:eos))

(cl:defpackage #:benchmark.eager-future2
  (:export #:run-benchmarks)
  (:use #:cl #:eager-future2))
