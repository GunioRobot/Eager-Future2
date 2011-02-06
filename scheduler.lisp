(in-package #:eager-future2)

(defvar *task-queue-lock* (make-lock "Eager Future2 thread pool lock"))
(defvar *leader-notifier* (make-condition-variable :name "Eager Future2 leader notifier"))
(defvar *task-queue* ())
(defvar *free-threads* 0)

(defvar *thread-counter-lock* (make-recursive-lock "Eager Future2 thread pool total thread counter lock"))
(defvar *total-threads* 0)

(defun make-pool-thread ()
  (make-thread
   (lambda ()
     (unwind-protect
          (catch 'die
            (let ((*debugger-hook* (lambda (c old-hook)
                                     (declare (ignore c old-hook))
                                     (throw 'continue nil))))
              (loop (catch 'continue
                      (funcall (with-lock-held (*task-queue-lock*)
                                 (incf *free-threads*)
                                 (unwind-protect
                                      (loop (if *task-queue*
                                                (return (pop *task-queue*))
                                                (condition-wait *leader-notifier* *task-queue-lock*)))
                                   (decf *free-threads*))))))))
       (with-recursive-lock-held (*thread-counter-lock*) (decf *total-threads*))))
   :name "Eager Future2 Worker")
  (with-recursive-lock-held (*thread-counter-lock*) (incf *total-threads*)))

(defun thread-pool-size ()
  (with-recursive-lock-held (*thread-counter-lock*)
    *total-threads*))

(defun advise-thread-pool-size (new-size)
  (with-recursive-lock-held (*thread-counter-lock*)
    (if (< *total-threads* new-size)
        (loop repeat (- new-size *total-threads*) do (make-pool-thread))
        (with-lock-held (*task-queue-lock*)
          (loop repeat (- *total-threads* new-size) do
               (push (lambda () (throw 'die nil)) *task-queue*)
               (condition-notify *leader-notifier*))))))

(eval-when (:load-toplevel)
  (advise-thread-pool-size 10))

(defun schedule-last (task)
  (with-lock-held (*task-queue-lock*)
    (setf *task-queue* (append *task-queue* (list task)))
    (when (< 0 *free-threads*)
      (condition-notify *leader-notifier*)))
  (values))

(defun schedule-immediate (task)
  (unless (with-lock-held (*task-queue-lock*)
            (when (< 0 *free-threads*)
              (setf *task-queue* (push task *task-queue*))
              (condition-notify *leader-notifier*)
              t))
    (make-thread task :name "Eager Future2 Temporary Worker"))
  (values))
