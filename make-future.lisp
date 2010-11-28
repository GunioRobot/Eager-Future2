(in-package #:eager-future2)

(defvar *default-future-type* :speculative
  "One of :eager, :speculative (default) or :lazy.
If eager, any newly created futures start their computation immediately.
If speculative, newly created futures are computed when thread pool threads are available, in FIFO future creation order.
If lazy, newly created futures are not computed until asked to yield their values.")

(defvar *computing-future* nil
  "Part of scheduling protocol for thread-pooled futures.")

(defun abort-scheduled-future-task (thread future-id)
  (when (thread-alive-p thread)
    (ignore-errors ;; should probably log them or something
      (interrupt-thread thread (lambda ()
                                 (when (eql *computing-future* future-id)
                                   (throw 'task-done nil)))))))

(defun make-scheduler-task (future-ptr)
  (lambda ()
    (catch 'task-done
      (flet ((get-future () (or (weak-pointer-value future-ptr) (throw 'task-done nil))))
        (let ((*computing-future* (future-id (get-future))))
          (with-lock-held ((lock (get-future)))
            (if (slot-boundp (get-future) 'values)
                (throw 'task-done nil)
                (setf (computing-thread (get-future)) (current-thread))))
          (finalize (get-future) (let ((thread (current-thread))
                                       (future-id *computing-future*))
                                   (lambda () (abort-scheduled-future-task thread future-id))))
          (let ((values (multiple-value-list (funcall (task (get-future))))))
            (apply #'force (get-future) values)))))))

(defun schedule-future (future future-type)
  (let ((scheduler-task (make-scheduler-task (make-weak-pointer future))))
    (ccase future-type
      (:eager (schedule-immediate scheduler-task))
      (:speculative (schedule-last scheduler-task)))))

(defun pcall (thunk &optional (future-type *default-future-type*))
  "Given a function of no arguments, returns an object (called a
future) that can later be used to retrieve the values computed by the
function.

future-type (by default the value of *default-future-type*) can either
be :eager, :speculative, or :lazy. See the documentation of
*default-future-type* for an explanation of the different future
types.

The function is called in an unspecified dynamic environment."
  (let ((future (make-future thunk (gensym "FUTURE-ID"))))
    (unless (eq future-type :lazy)
      (schedule-future future future-type))
    future))

(defmacro pexec (&body body)
  "A shorthand for (pcall (lambda () ...))."
  `(pcall (lambda () ,@body)))

(defmacro plet ((&rest bindings) &body body)
  "Like LET, but all bindings are evaluated asynchronously."
  (let ((bindings (mapcar (lambda (x) (if (consp x) x (list x nil)))
                          bindings)))
    (let ((syms (mapcar (lambda (x) (gensym (string (car x))))
                        bindings)))
      `(let ,(loop for (nil exp) in bindings
                for sym in syms
                collect `(,sym (pexec ,exp)))
         (symbol-macrolet ,(loop for (var nil) in bindings
                              for sym in syms
                              collect `(,var (yield ,sym)))
           ,@body)))))
