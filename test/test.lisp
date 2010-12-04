;;; The contents of this file are released into the public domain.

(in-package #:test.eager-future2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite eager-future2))

(in-suite eager-future2)

(defun run-tests ()
  (run! 'eager-future2))

(test sanity
      (is (equal '(1 2 3) (yield (pexec (list 1 2 3)))))
      (let ((task (pexec (+ 4 2))))
        (sleep .01)
        (is (= 6 (yield task)))))

;; crashes on CCL for large (~50-100) values of loop repeat
(test stress
  (flet ((compute ()
           (loop :for i :from 0 :below 100000
                 :sum (* i i))))
      (let ((tasks (loop repeat 40 collect (pcall #'compute)))
            (answer (compute)))
        (sleep .05)
        (is (every (lambda (tsk) (= (yield tsk) answer)) tasks)))))

(test multi-yield
      (let* ((task (pexec (sleep .1) :ok))
             (yielders (loop :for i :from 0 :below 10
                          :collect (pexec (yield task)))))
        (sleep .01)
        (is (every (lambda (tsk) (eq (yield tsk) :ok)) yielders))))

(test plet
      (plet ((x (list 1 2))
             (y (list 3 4)))
            (sleep .01)
            (is (equal '(1 2 3 4) (append x y)))))

(test delayed-signal
      (let ((task (pexec (error "Wrong!"))))
        (sleep .01)
        (signals error (yield task))))

(test select-random
      (is (member (yield (select (pexec (sleep (random 0.2)) 1)
                                 (pexec (sleep (random 0.2)) 2)
                                 (pexec (sleep (random 0.2)) 3))) '(1 2 3))))

(test select-always
      (is (= 2 (yield (select (pexec (sleep 0.05) 1) (pexec 2))))))

(test select-all-tasks-done
      (flet ((make-done-task (val)
               (let ((task (pexec val)))
                 (yield task)
                 task)))
        (is (= 1 (yield (apply #'select (mapcar #'make-done-task '(1 2 3))))))))

(test select-error
      (signals error (yield (select (pexec (sleep 0.01) 1)
                                    (pexec (error "Error"))))))

(test select-signal-wakeup
      (signals error (yield (select (pexec (sleep 1) (error "foo")))))) ;; make sure errors wake up threads sleeping on this future

(test force
  (is (equal 1
             (let* ((flag t)
                    (foo (pexec (sleep 0.1) (setf flag nil) 'foo))
                    (bar (pexec (sleep 5) 'bar)))
               (force foo 1)
               (sleep 0.2)
               (and flag (yield foo))))))

(test gc-future
  (is (= 1
         (let ((foo 1))
           (pexec (sleep 1) (setf foo 2))
           (loop repeat 10 do (trivial-garbage:gc :full t))
           (sleep 2)
           foo))))

(test select-time-sanity
      (is (= 4
             (yield (select (pcall (lambda () (sleep 5)) :eager) (pexec 4))))))

(test select-timeout-sanity
      (is (= 4
             (yield (select-timeout 5 (pexec 4))))))

(test thread-pool-size-advise
  (trivial-garbage:gc :full t)
  (is (= 0
         (progn (advise-thread-pool-size 0)
                (thread-pool-size))))
  (is (= 10
         (progn (advise-thread-pool-size 10)
                (thread-pool-size)))))

(test pfuncall-is-parallel
  (is (> 6.1
         (- (prog1 (get-universal-time)
              (pfuncall #'+ (progn (sleep 5) 1) (progn (sleep 5) 2) (progn (sleep 5) 3)))
            (get-universal-time)))))

(test por1
  (is (member (por 5 6 nil) '(5 6))))

(test por2
  (is (= 6 (por (progn (sleep 1) 5) (progn (sleep 0.5) 6) nil))))
