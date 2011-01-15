(in-package #:eager-future2)

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

(defun iterate-futures (proc futures)
  "Calls proc on each future in select order. Proc is a procedure that
takes the currently yieldable future as its first argument, and the
list of futures not yet processed as its second. Futures need to be
yielded by proc - this is done so that proc can have control of error
handling. The second argument is useful to for example be able to
terminate remaining computations before a non-local control transfer."
  (loop while futures do
       (let* ((next (apply #'select futures))
              (remaining (remove next futures)))
         (funcall proc next remaining)
         (setf futures remaining))))

(defun force-all (futures &rest values)
  "Calls force on all given future with values. Useful to stop
remaining computations in for example iterate-futures."
  (map nil (lambda (future) (apply #'force future values)) futures))

(defmacro pand (&rest exprs)
  "Evaluates expressions in parallel. If any expression yields nil,
terminates all outstanding computations and returns nil. If all
expressions yield a non-nil value, returns t."
  (let ((tag (gensym)))
    `(block ,tag
       (iterate-futures (lambda (x remaining)
                          (unless (yield x)
                            (force-all remaining nil)
                            (return-from ,tag nil)))
                        (list ,@(loop for x in exprs collect `(pexec ,x))))
       t)))

(defmacro por (&rest exprs)
  "Evaluates expressions in parallel. Returns the value of the first
expression that yields a non-nil value. If all expressions evaluate
to nil, returns nil."
  (let ((tag (gensym)))
    `(block ,tag
       (iterate-futures (lambda (x remaining)
                          (let ((value (yield x)))
                            (when value
                              (force-all remaining nil)
                              (return-from ,tag value))))
                        (list ,@(loop for x in exprs collect `(pexec ,x)))))))

(defun select-timeout (timeout &rest futures)
  "Given a timeout (in seconds) and a set of futures, returns either
nil if no futures are ready to yield when timeout seconds have
elapsed, or the first yieldable future otherwise."
  (let* ((timeout-future (pcall (lambda () (sleep timeout)) :eager))
         (first-future (apply #'select timeout-future futures)))
    (unless (eq timeout-future first-future)
      first-future)))

(defmacro pfuncall (function &rest args)
  "Evaluates args in parallel before funcalling the given function on them."
  (let ((syms (loop repeat (length args) collect (gensym))))
    `(plet ,(loop for s in syms for arg in args collect (list s arg))
       (funcall ,function ,@syms))))

(defun touch (x)
  "If x is a future, yields its value, otherwise returns x.
Borrowed from MultiLisp."
  (if (typep x 'future)
      (yield x)
      x))
