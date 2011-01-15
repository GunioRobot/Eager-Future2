(cl:defpackage #:eager-future2
  (:use #:cl #:bordeaux-threads #:trivial-garbage)
  (:export

   ;; type
   #:future

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
   #:iterate-futures
   #:force-all
   #:pand
   #:por
   #:select-timeout
   #:pfuncall
   #:touch
   ))
