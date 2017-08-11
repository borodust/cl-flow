(in-package :cl-flow.tests)


(define-condition skipping-condition (simple-error) ())

(define-condition recoverable-condition (simple-error) ())


(defun invoke (fn)
  (handler-bind ((skipping-condition #'skip-flow-block)
                 (recoverable-condition (lambda (e)
                                          (declare (ignore e))
                                          (use-value -1))))
    (funcall fn)))


(defvar *dispatcher* (simple-flow-dispatcher:make-simple-dispatcher :threads 4
                                                                    :invoker #'invoke))

(defun run-it (flow)
  (run *dispatcher* flow))
