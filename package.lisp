(cl:defpackage #:eager-future2
  (:use #:cl #:bordeaux-threads #:trivial-garbage)
  (:export #:yield #:ready-to-yield? #:select #:force
           #:advise-thread-pool-size
           ))
