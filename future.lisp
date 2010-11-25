(in-package #:eager-future2)

(defclass future ()
  ((values :accessor %values)
   (task :accessor task :initarg :task)
   (lock :reader lock :initform (make-lock "future lock"))
   (computing-thread :accessor computing-thread :initform nil)
   (wait-list :accessor wait-list :initform ())
   (interrupt-tag :reader interrupt-tag :initarg :interrupt-tag)))

(defun ready-to-yield? (future)
  "Returns t if the future values have been computed, nil otherwise."
  (with-lock-held ((lock future))
    (slot-boundp future 'values)))

(defvar %computing-future nil) ;; protocol for random pool threads

(defun force (future &rest values)
  "If the future has not yet yielded a value, installs the given
values as the yield-values of the future (stopping any ongoing
computation of the future)."
  (with-lock-held ((lock future))
    (if (slot-boundp future 'values)
        (return-from force nil)
        (setf (%values future) values)))
  (dolist (x (wait-list future))
    (with-lock-held ((car x))
      (condition-notify (cdr x))))
  (with-slots (computing-thread) future
    (when (and (not (eq computing-thread (current-thread))) (thread-alive-p computing-thread))
      (ignore-errors ;; should probably log them or something
        (interrupt-thread computing-thread
                          (lambda ()
                            (when (eq %computing-future (interrupt-tag future))
                              (throw 'task-done nil))))))
    (setf computing-thread nil
          (wait-list future) nil
          (task future) nil)))

(defun select (&rest futures)
  "Returns the first future that is ready to yield."
  (let ((notifier (make-condition-variable))
        (select-lock (make-lock "select lock")))
    (with-lock-held (select-lock)
      (let (any-computing?)
        (dolist (future futures)
          (with-lock-held ((lock future))
            (when (slot-boundp future 'values)
              (return-from select future))
            (when (and (not any-computing?) (computing-thread future))
              (setf any-computing? t))
            (push (cons select-lock notifier) (wait-list future))))
        (unless any-computing?
          (schedule-future (first futures))))
      (loop (dolist (future futures)
              (with-lock-held ((lock future))
                (when (slot-boundp future 'values)
                  (return-from select future))))
         (condition-wait notifier select-lock)))))

(defun yield (future)
  "Returns the computed value of the future.

In case of a delayed future, computes the value of the future in the
current thread.

In case of a speculative future, if no other thread is computing the
value of the future, computes the value in the current thread. If
another thread is currently computing the value of the future, blocks
until the value is available.

In case of an eager future, blocks until the value is available."
  (tagbody
     (with-lock-held ((lock future))
       (if (slot-boundp future 'values)
           (go done)
           (if (computing-thread future)
               (go select)
               (progn (setf (computing-thread future) (current-thread))
                      (go compute)))))
     select (select future) (go done)
     compute (multiple-value-call #'force future (funcall (task future)))
     done (values-list (%values future))))

;;; implementations

;;; ask for a delayed future - get a future right back
;;; ask for an eager future - schedule a future to run immediately and give you the future object
;;; ask for a speculative future - put future on work queue and return the future


