(cl:defpackage #:eager-future2
  (:use #:cl #:bordeaux-threads #:trivial-garbage)
  (:export

   ;; making futures
   #:*default-future-type*
   #:pcall

   ;; dealing with futures
   #:ready-to-yield?
   #:yield
   #:select
   #:force
   #:force-values ;; restart

   ;; thread pool management
   #:thread-pool-size
   #:advise-thread-pool-size

   ;; helper functions
   #:pexec
   #:plet
   ))
