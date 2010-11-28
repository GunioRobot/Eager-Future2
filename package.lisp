(cl:defpackage #:eager-future2
  (:use #:cl #:bordeaux-threads #:trivial-garbage)
  (:export

   ;; making futures
   #:*default-future-type*
   #:pcall
   #:pexec
   #:plet

   ;; dealing with futures
   #:ready-to-yield?
   #:yield
   #:select
   #:force

   ;; thread pool management
   #:thread-pool-size
   #:advise-thread-pool-size
   ))
