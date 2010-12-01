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

;;(defun pand)

;;(defun por)

(defun select-timeout (timeout &rest futures)
  (apply #'select (pcall (lambda () (sleep timeout)) :eager) futures))