(in-package #:eager-future2)

(defvar *thread-pool-lock* (make-lock "Eager Future2 thread pool lock"))
(defvar *leader-notifier* (make-condition-variable :name "Eager Future2 leader notifier"))
(defvar *free-threads* 0)
(defvar *waiting-tasks* ())

(defvar *thread-counter-lock* (make-lock "Eager Future2 thread pool counter lock"))
(defvar *total-threads* 0)

(defun make-pool-thread ()
  (make-thread
   (lambda ()
     (unwind-protect
          (catch 'die
            (let ((*debugger-hook* (lambda (c old-hook)
                                     (declare (ignore c old-hook))
                                     (throw 'continue nil))))
              (loop (with-lock-held (*thread-pool-lock*)
                      (incf *free-threads*)
                      (catch 'continue
                        (funcall (loop (let ((task (pop *waiting-tasks*)))
                                         (if task
                                             (progn
                                               (decf *free-threads*)
                                               (return task))
                                             (condition-wait *leader-notifier* *thread-pool-lock*))))))))))
       (with-lock-held (*thread-counter-lock*) (decf *total-threads*))))
   :name "Eager Future2 Worker")
  (with-lock-held (*thread-counter-lock*) (incf *total-threads*)))

(defun thread-pool-size ()
  (with-lock-held (*thread-counter-lock*)
    *total-threads*))

(defun advise-thread-pool-size (new-size)
  (with-lock-held (*thread-counter-lock*)
    (if (< *total-threads* new-size)
        (loop repeat (- new-size *total-threads*) do (make-pool-thread))
        (with-lock-held (*thread-pool-lock*)
          (loop repeat (- *total-threads* new-size) do
               (push (lambda () (throw 'die nil)) *waiting-tasks*))))))

(eval-when (:load-toplevel)
  (advise-thread-pool-size 10))

(defun schedule-last (task)
  (with-lock-held (*thread-pool-lock*)
    (setf *waiting-tasks* (append *waiting-tasks* (list task)))
    (condition-notify *leader-notifier*))
  (values))

(defun schedule-immediate (task)
  (unless (with-lock-held (*thread-pool-lock*)
            (when (< 0 *free-threads*)
              (setf *waiting-tasks* (push task *waiting-tasks*))
              (condition-notify *leader-notifier*)
              t))
    (make-thread task :name "Eager Future2 Temporary Worker"))
  (values))
