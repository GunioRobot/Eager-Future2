;;; The contents of this file are released into the public domain.

(in-package #:benchmark.eager-future2)

;;; benchmarks from Appendix A of Marc Feeley's PhD dissertation:
;;; Marc Feeley. An Efficient and General Implementation of Futures on
;;; Large Scale Shared-Memory Multiprocessors. PhD thesis, Brandeis
;;; University, April 1993.
;;; http://www.iro.umontreal.ca/~feeley/papers/FeeleyPhD.pdf

;; abisort

;; This program sorts 16384 integers using the adaptive bitonic sort
;; algorithm described in Gianfranco Bilardi, Alexandru Nicolau:
;; Adaptive Bitonic Sorting: An Optimal Parallel Algorithm for
;; Shared-Memory Machines. SIAM J. Comput. 18(2): 216-228 (1989)

(defstruct node
  left right value)

(defun compare-and-swap (node1 node2 up? true-cont false-cont)
  (let ((v1 (node-value node1))
        (v2 (node-value node2)))
    (if (or (and up? (>= v1 v2)) (< v1 v2))
        (progn (setf (node-value node1) v2
                     (node-value node2) v1)
               (funcall true-cont))
        (funcall false-cont))))

(defun fixup-tree-1 (pl pr up?)
  (when pl
    (compare-and-swap pl pr up?
                      ;; swap right subtrees, search path goes left
                      (lambda ()
                        (rotatef (node-right pl) (node-right pr))
                        (fixup-tree-1 (node-left pl) (node-left pr) up?))
                      ;; search path goes right
                      (lambda ()
                        (fixup-tree-1 (node-right pl) (node-right pr) up?)))))

(defun fixup-tree-2 (pl pr up?)
  (when pl
    (compare-and-swap pl pr up?
                      ;; swap right subtrees, search path goes left
                      (lambda ()
                        (rotatef (node-left pl) (node-left pr))
                        (fixup-tree-2 (node-right pl) (node-right pr) up?))
                      ;; search path goes right
                      (lambda ()
                        (fixup-tree-2 (node-left pl) (node-left pr) up?)))))

(defun pbimerge (root spare up?)
  (compare-and-swap root spare up?
                    (lambda () (fixup-tree-1 (node-left root) (node-right root) up?))
                    (lambda () (fixup-tree-2 (node-left root) (node-right root) up?)))
  (when (node-left root)
    (let ((left-half (pexec (pbimerge (node-left root) root up?))))
      (pbimerge (node-right root) spare up?)
      (yield left-half))))

(defun pbisort-up (root spare)
  (let ((left (node-left root)))
    (when left
      (let ((left-half (pexec (pbisort-up left root))))
        (pbisort-down (node-right root) spare)
        (yield left-half)
        (pbimerge root spare t)))
    (compare-and-swap root spare t (lambda () t) (lambda () nil))))

(defun pbisort-down (root spare)
  (let ((left (node-left root)))
    (when left
      (let ((left-half (pexec (pbisort-down left root))))
        (pbisort-up (node-right root) spare)
        (yield left-half)
        (pbimerge root spare nil)))
    (compare-and-swap root spare nil (lambda () t) (lambda () nil))))

(defun make-inorder-tree (depth)
  (labels ((tree-loop (i depth)
             (if (= depth 1)
                 (cons (make-node :left nil :right nil :value i) i)
                 (let* ((x (tree-loop i (- depth 1)))
                        (l-tree (car x))
                        (l-imax (cdr x)))
                   (let* ((y (tree-loop (+ l-imax 2) (- depth 1)))
                          (r-tree (car y))
                          (r-imax (cdr y)))
                     (cons (make-node :left l-tree :right r-tree :value (+ l-imax 1)) r-imax))))))
    (tree-loop 0 depth)))

(defun abisort-bench (&optional (k 14))
  (let ((x (make-inorder-tree k)))
    (pbisort-up (car x) (make-node :left nil :right nil :value (+ (cdr x) 1)))))

;; allpairs

;; This program computes the shortest paths between all pairs of 117 nodes using
;; a parallel version of Floyd's algorithm.

(defmacro p-dotimes ((var upto) &body body)
  (let ((hi (gensym)) (mid (gensym)) (lo-half (gensym)))
    `(labels ((p-dotimes-do (,var ,hi)
                (if (= ,var ,hi)
                    (progn ,@body)
                    (let* ((,mid (floor (+ ,var ,hi) 2))
                           (,lo-half (pexec (p-dotimes-do ,var ,mid))))
                      (p-dotimes-do (+ ,mid 1) ,hi)
                      (yield ,lo-half)))))
       (p-dotimes-do 0 ,upto))))

(defun apsp/par (a n)
  (do ((k 0 (+ k 1)))
      ((= k n))
    (let ((k*n (* k n)))
      (p-dotimes (i (1- n))
        (let* ((i*n (* i n))
               (i*n+k (+ i*n k)))
          (do ((j 0 (+ j 1)))
              ((= j n))
            (let* ((kpath (+ (aref a i*n+k) (aref a (+ k*n j))))
                   (i*n+j (+ i*n j)))
              (when (< kpath (aref a i*n+j))
                (setf (aref a i*n+j) kpath)))))))))

(defun make-linear-adjacency-matrix (n)
  (let ((a (make-array (* n n) :initial-element (floor most-positive-fixnum 2))))
    (setf (aref a 0) 0)
    (do ((i 1 (+ i 1)))
        ((= i n))
      (setf (aref a (+ (* i n) i))        0
            (aref a (+ (* (- i 1) n) i))  1
            (aref a (+ (* i n) (- i 1)))  1))
    a))

(defun allpairs-benchmark (&optional (n 117))
  (apsp/par (make-linear-adjacency-matrix n) n))

;; fib

;;This program computes $F_{25}$, the 25th fibonacci number, using
;;the ``standard'' doubly recursive algorithm.

(defun pfib (n)
  (if (< n 2)
      n
      (let* ((f1 (pexec (pfib (- n 1))))
             (f2 (pfib (- n 2))))
        (+ (yield f1) f2))))

(defun pfib-benchmark (&optional (n 25))
  (pfib n))

;; mm

;; This program multiplies two matrices of integers (50 by 50).

(defun mm (n m1 m2 m3)                    ; m1 * m2 -> m3
  (labels
      ((compute-entry (row col)       ; loop to compute inner product
         (labels ((compute-loop (i j sum)
                    (if (>= j 0)
                        (compute-loop (- i 1) (- j n) (+ sum (* (aref m1 i) (aref m2 j))))
                        (setf (aref m3 (+ i 1 col)) sum))))
           (compute-loop (+ row n -1) (+ (* n (1- n)) col) 0)))

       (compute-cols-between (row i j) ; DAC over columns
         (if (= i j)
             (compute-entry row i)
             (let* ((mid (floor (+ i j) 2))
                    (half1 (pexec (compute-cols-between row i mid))))
               (compute-cols-between row (+ mid 1) j)
               (yield half1))))

       (compute-rows-between (i j)     ; DAC over rows
         (if (= i j)
             (compute-cols-between (* i n) 0 (- n 1))
             (let* ((mid (floor (+ i j) 2))
                    (half1 (pexec (compute-rows-between i mid))))
               (compute-rows-between (+ mid 1) j)
               (yield half1)))))

    (compute-rows-between 0 (1- n))))

(defun mm-benchmark (&optional (n 50))
  (mm n (make-array (* n n) :initial-element 2)
      (make-array (* n n) :initial-element 2)
      (make-array (* n n) :initial-element nil)))

;; mst

;; This program computes the minimum spanning tree of a 1000 node graph.  A
;; parallel version of Prim's algorithm is used.

(defstruct city
  x y closest distance)

(defun prim (cities ncities)
  (let* ((max-i (- ncities 1))
         (target0 (aref cities max-i)))
    (setf (city-closest target0) target0) ;; makes drawing easier
    (labels ((prim-loop (i target)
               (if (= i 0)
                   (add-last-city (aref cities 0) target)
                   (let* ((closest-i (find-closest-city/ptree cities i target))
                          (newcity (aref cities closest-i)))
                     (setf (aref cities closest-i) (aref cities i)
                           (aref cities i) newcity)
                     (prim-loop (1- i) newcity)))))
      (prim-loop (1- max-i) target0))))

(defun add-last-city (city newcity)
  (let* ((newdist (distance city newcity))
         (olddist (city-distance city)))
    (when (< newdist olddist)
      (setf (city-distance city) newdist
            (city-closest city) newcity))))

(defun distance (c1 c2)
  (let ((dx (- (city-x c1) (city-x c2)))
        (dy (- (city-y c1) (city-y c2))))
    (+ (* dx dx) (* dy dy))))

(defun combine-interval/ptree (lo hi f combine)
  (let* ((n (1+ (- hi lo)))
         (adjust (1- lo))
         (first-leaf (floor (1+ n) 2))
         (treeval
          (labels ((combine-loop (i)
                     (if (< i first-leaf)
                         (let* ((left (pexec (combine-loop (* i 2))))
                                (right (funcall combine
                                                (combine-loop (1+ (* i 2)))
                                                (funcall f (+ i adjust)))))
                           (funcall combine right (yield left)))
                         (funcall f (+ i adjust)))))
            (combine-loop 1))))
    (if (evenp n)
        (funcall combine treeval (funcall f hi))
        treeval)))

(defun find-closest-city/ptree (cities max-i newcity)
  (combine-interval/ptree 0 max-i
    (lambda (i) (update-city i cities newcity))
    (lambda (i1 i2)
      (if (< (city-distance (aref cities i1))
             (city-distance (aref cities i2)))
          i1
          i2))))

(defun update-city (i cities newcity)
  (let* ((city (aref cities i))
         (newdist (distance city newcity))
         (olddist (city-distance city)))
    (when (< newdist olddist)
      (setf (city-distance city) newdist
            (city-closest city) newcity))
    i))

(defun make-random-vector-of-cities (n)
  (flet ((random-coordinate () (floor (random 3434534) 1000)))
    (let ((cities (make-array n)))
      (dotimes (i n)
        (setf (aref cities i) (make-city :x (random-coordinate)
                                         :y (random-coordinate)
                                         :closest ()
                                         :distance most-positive-fixnum)))
      cities)))

(defun mst-benchmark (&optional (n 1000))
  (prim (make-random-vector-of-cities n) n))

;; poly

;; This program computes the square of a 200 term polynomial of $x$ (with
;; integer coefficients) and evaluates the resulting polynomial for a
;; certain value of $x$.

(defun poly* (p1 p2)                    ; compute p1*p2
  (when (and p1 p2)
    (poly+*k (cons 0 (poly* p1 (cdr p2)))
             p1
             (car p2))))

(defun poly+*k (p1 p2 k)                ; compute p1+p2*k
  (if p2
      (if p1
          (let ((rest (pexec (poly+*k (touch (cdr p1)) (cdr p2) k))))
            (cons (+ (car p1) (* (car p2) k)) rest))
          (let ((rest (pexec (poly+*k '() (cdr p2) k))))
            (cons (* (car p2) k) rest)))
      p1))

(defun poly-eval (p x)                  ; compute value of p at x
  (labels ((poly-eval-loop (p y sum)
             (if p
                 (poly-eval-loop (yield (cdr p)) (* x y) (+ sum (* (car p) y)))
                 sum)))
    (poly-eval-loop p 1 0)))

(defun poly-benchmark (&optional (number-terms 200))
  (let ((p (make-list number-terms :initial-element 1)))
    (poly-eval (poly* p p) 1)))

;; qsort

;; This program sorts a list of 1000 integers using a parallel version of
;; the Quicksort algorithm.

(defun qsort (lst)
  (labels ((pfilter (test lst)
             (let ((lst (touch lst)))
               (when (consp lst)
                 (if (funcall test (car lst))
                     (cons (car lst) (pexec (pfilter test (cdr lst))))
                     (pfilter test (cdr lst))))))
           (qs (lst tail)
             (if (consp lst)
                 (let* ((pivot (car lst))
                        (sorted-larger (pexec (qs (pfilter (lambda (x) (>= x pivot)) (cdr lst))
                                                  tail))))
                   (qs (pfilter (lambda (x) (< x pivot)) (cdr lst))
                       (cons pivot sorted-larger)))
                 tail)))
    (qs lst ())))

(defun qsort-benchmark (&optional (n 1000))
  (let ((array (make-array n)))
    (dotimes (i n)
      (setf (aref array i) i))
    (loop for i from (1- (length array)) downto 1 do
         (let ((j (random (1+ i))))
           (rotatef (aref array j) (aref array i))))
    (labels ((collect-results (x)
               (let ((x (touch x)))
                 (if (consp x)
                     (cons (car x) (collect-results (cdr x)))
                     x))))
      (collect-results (qsort (coerce array 'list))))))

;; nqueens

;; This program computes the number of solutions to the n-queens problem for n=10.

(defun nqueens-benchmark (&optional (n 10))
  (labels ((nqueens (rows-left
                     free-diag1        ;all bits set
                     free-diag2
                     free-cols) ;bits 0 to n-1 set
             (let ((free (logand free-cols free-diag1 free-diag2)))
               (labels ((nqueens-inner (col)
                          (cond ((> col free)
                                 0)
                                ((= (logand col free) 0)
                                 (nqueens-inner (* col 2)))
                                ((= rows-left 1)
                                 (1+ (nqueens-inner (* col 2))))
                                (t
                                 (let ((sub-solns (pexec (nqueens (- rows-left 1)
                                                                  (1+ (ash (- free-diag1 col) 1))
                                                                  (ash (- free-diag2 col) -1)
                                                                  (- free-cols col))))
                                       (other-solns (nqueens-inner (* col 2))))
                                   (+ (yield sub-solns) other-solns))))))
                 (nqueens-inner 1)))))
    (nqueens n -1 -1 (1- (ash 1 n)))))

;; rantree

;; This program models the traversal of a random binary tree with on the
;; order of 32768 nodes.  The branching factor is 50%.

(defun lehmer-left (seed)
  (+ 1 (* seed #xface475)))
(defun lehmer-right (seed)
  (+ 1 (* seed #x283feed)))

(defun rantree-benchmark (&optional (n 32768))
  (labels ((prt-loop (n seed)
             (cond ((<= n 2)
                    n)
                   ((> seed 0)
                    (let* ((ln (+ 1 (mod seed (- n 2))))
                           (rn (- (- n 1) ln))
                           (left (pexec (prt-loop ln (lehmer-left seed))))
                           (right (prt-loop rn (lehmer-right seed))))
                      (+ (yield left) (+ right 1))))
                   (t
                    (+ 1 (prt-loop (- n 1) (lehmer-left seed)))))))
    (prt-loop n 1)))

;; scan

;; This program computes the parallel prefix sum of a vector of 32768
;; integers.  The vector is modified in place.  A given element is
;; replaced by the sum of itself and all preceding elements.

(defun scan (c v)
  (let ((n (length v)))
    (labels ((pass1 (i j)
               (if (< i j)
                   (let* ((m      (floor (+ i j) 2))
                          (left   (pexec (pass1 i m)))
                          (right  (pass1 (+ m 1) j))
                          (result (+ (yield left) right)))
                     (setf (aref v j) result)
                     result)
                   (aref v j)))
             (pass2 (i j c)
               (if (< i j)
                   (let* ((m     (floor (+ i j) 2))
                          (left  (pexec (pass2 i m c)))
                          (cc    (+ c (aref v m))))
                     (pass2 (+ m 1) j cc)
                     (setf (aref v m) cc)
                     (yield left)))))
      (when (> n 0)
        (let ((j (1- n)))
          (pass1 0 j)
          (pass2 0 j c)
          (setf (aref v j) (+ c (aref v j))))))))

(defun scan-benchmark (&optional (n 32768))
  (scan 0 (make-array n :initial-element 0)))

;; sum

;; This program computes the sum of a vector of 32768 integers.

(defun psum (vect l h)                   ; sum vector from 'l' to 'h'
  (if (= l h)
      (aref vect l)
      (let* ((mid (floor (+ l h) 2))
             (lo  (pexec (psum vect l mid)))
             (hi  (psum vect (+ mid 1) h)))
        (+ (yield lo) hi))))

(defun psum-benchmark (&optional (n 32768))
  (psum (make-array n :initial-element 1) 0 (- n 1)))

;; tridiag

;; This program solves a tridiagonal system of 32767 equations.

(defstruct (abcyx (:conc-name nil))
  a b c y x)

(defun reduce/par (equ imid)
  (labels ((reduce-equation (i delta)
             (let* ((equ-ileft  (aref equ (- i delta)))
                    (equ-iright (aref equ (+ i delta)))
                    (equ-i (aref equ i))
                    (e (- (floor (a equ-i) (b equ-ileft))))
                    (f (- (floor (c equ-i) (b equ-iright)))))
               (setf (a equ-i) (* e (a equ-ileft))
                     (c equ-i) (* f (c equ-iright))
                     (b equ-i) (+ (b equ-i)
                                  (+ (* e (c equ-ileft))
                                     (* f (a equ-iright))))
                     (y equ-i) (+ (y equ-i)
                                  (+ (* e (y equ-ileft))
                                     (* f (y equ-iright)))))))
           (do-branch (i delta)
             (if (= delta 1)
                 (reduce-equation i delta)
                 (let* ((ileft  (- i delta))
                        (iright (+ i delta))
                        (l (pexec (do-branch ileft (floor delta 2)))))
                   (do-branch iright (floor delta 2))
                   (yield l)
                   (do ((d 1 (* d 2)))
                       ((> d delta))
                     (reduce-equation i d))))))
    (do-branch imid (floor imid 2))))

(defun backsolve/par (equ imid)
  (labels ((backsolve-loop (i delta)
             (let ((equ-i (aref equ i)))
               (setf (x equ-i) (floor (- (y equ-i)
                                         (+ (* (a equ-i)
                                               (x (aref equ (- i delta))))
                                            (* (c equ-i)
                                               (x (aref equ (+ i delta))))))
                                      (b equ-i)))
               (when (> delta 1)
                 (let* ((new-delta (floor delta 2))
                        (l (pexec (backsolve-loop (- i new-delta) new-delta))))
                   (backsolve-loop (+ i new-delta) new-delta)
                   (yield l))))))
    (backsolve-loop imid imid)))

(defun tridiag-benchmark (&key (k 15))
  (let* ((n+1 (expt 2 k))
         (n (- n+1 1))
         (imid (floor n+1 2))
         (equ (let ((x (make-array (+ n 2))))
                (dotimes (i (length x))
                  (setf (aref x i) (make-abcyx)))
                x)))
    (do ((i (+ n 1) (- i 1)))
        ((< i 0))
      (let ((equ-i (aref equ i)))
        (setf (a equ-i) 1
              (b equ-i) 1
              (c equ-i) 1
              (y equ-i) 3
              (x equ-i) 0)))
    (let ((equ-1 (aref equ 1)))
      (setf (a equ-1) 0
            (b equ-1) 1
            (c equ-1) 1
            (y equ-1) 2))
    (let ((equ-n (aref equ n)))
      (setf (a equ-n) 1
            (b equ-n) 1
            (c equ-n) 0
            (y equ-n) 2))

    (reduce/par equ imid)
    (backsolve/par equ imid)))

;;; run benchmark suite

(defun run-benchmarks ()
  (abisort-bench)
  (allpairs-benchmark)
  (pfib-benchmark)
  (mm-benchmark)
  (mst-benchmark)
  (poly-benchmark)
  (qsort-benchmark)
  (nqueens-benchmark)
  (rantree-benchmark)
  (scan-benchmark)
  (psum-benchmark)
  (tridiag-benchmark))
