(in-package #:eager-future2)

(defclass future ()
  ((values :accessor %values)
   (task :accessor task :initarg :task)
   (lock :reader lock :initform (make-lock "future lock"))
   (computing-thread :accessor computing-thread :initform nil)
   (wait-list :accessor wait-list :initform ())
   (future-id :reader future-id :initarg :future-id)
   (restart-notifier :accessor restart-notifier :initform nil)
   (error-descriptor :accessor error-descriptor :initform nil)
   (proxy-restart :accessor proxy-restart :initform nil)))

(defun make-future (task future-id)
  (make-instance 'future :task task :future-id future-id))

(defun %ready-to-yield? (future)
  (or (slot-boundp future 'values) (error-descriptor future)))

(defun ready-to-yield? (future)
  "Returns non-nil if the future values have been computed, nil otherwise."
  (with-lock-held ((lock future))
    (%ready-to-yield? future)))

(defun force (future &rest values)
  "If the future has not yet yielded a value, installs the given
values as the yield-values of the future (stopping any ongoing
computation of the future)."
  (with-lock-held ((lock future))
    (if (slot-boundp future 'values)
        (return-from force nil)
        (setf (%values future) values)))
  (with-slots (computing-thread) future
    (unless (eq computing-thread (current-thread))
      (abort-scheduled-future-task computing-thread (future-id future)))
    (dolist (x (wait-list future))
      (with-lock-held ((car x))
        (condition-notify (cdr x))))
    (setf computing-thread nil
          (wait-list future) nil
          (task future) nil
          (restart-notifier future) nil
          (error-descriptor future) nil
          (proxy-restart future) nil)))

(defun select (&rest futures)
  "Returns the first future that is ready to yield."
  (let ((notifier (make-condition-variable :name "Eager Future2 select notifier"))
        (select-lock (make-lock "Eager Future2 select lock")))
    (with-lock-held (select-lock)
      (let (any-computing?)
        (dolist (future futures)
          (with-lock-held ((lock future))
            (when (%ready-to-yield? future)
              (return-from select future))
            (when (and (not any-computing?) (computing-thread future))
              (setf any-computing? t))
            (push (cons select-lock notifier) (wait-list future))))
        (unless any-computing?
          (schedule-future (first futures) :speculative)))
      (loop (dolist (future futures)
              (when (ready-to-yield? future)
                (return-from select future)))
         (condition-wait notifier select-lock)))))

(defun yield (future)
  "Returns the computed values of the future.

In case of a delayed future, computes the value of the future in the
current thread.

In case of a speculative future, if no other thread is computing the
value of the future, computes the value in the current thread. If
another thread is currently computing the value of the future, blocks
until the value is available.

In case of an eager future, blocks until the value is available."
  (assert (typep future 'future) () "~A is not a future" future)
  (tagbody (catch 'task-done
             (let ((*computing-future* (future-id future)))
               (with-lock-held ((lock future))
                 (cond ((slot-boundp future 'values) (return-from yield (values-list (%values future))))
                       ((error-descriptor future) (go handle-error))
                       ((computing-thread future) (go select))
                       (t (setf (computing-thread future) (current-thread)))))
               (multiple-value-call #'force future (restart-case (funcall (task future))
                                                     (force-values (&rest values)
                                                       :report "Set future value"
                                                       :interactive (lambda () (list (eval (read))))
                                                       (values-list values))))))
     (return-from yield (values-list (%values future)))
   select (select future)
   handle-error (with-lock-held ((lock future))
                  (if (slot-boundp future 'values)
                      (return-from yield (values-list (%values future)))
                      (progn
                        (catch 'go-select
                          (eval `(restart-case (error ,(car (error-descriptor future)))
                                   (force-values (&rest values)
                                     :report "Set future value"
                                     :interactive (lambda () (list (eval (read))))
                                     (setf (proxy-restart ,future) (cons 'force-values values)
                                           (error-descriptor ,future) nil)
                                     (condition-notify (restart-notifier ,future))
                                     (throw 'go-select nil))
                                   ,@(loop for restart in (cdr (error-descriptor future)) collect
                                          `(,(restart-name restart) (&rest args)
                                             (setf (proxy-restart ,future) (cons ,restart args)
                                                   (error-descriptor ,future) nil)
                                             (condition-notify (restart-notifier ,future))
                                             (throw 'go-select nil))))))
                        (go select))))))
