(cl:in-package :cl-flow.tests)


(define-condition skipping-condition (simple-error) ())

(define-condition recoverable-condition (simple-error) ())


(defun invoke (fn)
  (handler-bind ((skipping-condition (lambda (e)
                                       (declare (ignore e))
                                       (skip-flow-block)))
                 (recoverable-condition (lambda (e)
                                          (declare (ignore e))
                                          (use-flow-block-value -1))))
    (funcall fn)))


(defun run-it (flow)
  (let ((dispatcher (simple-flow-dispatcher:make-simple-dispatcher :threads 4
                                                                   :invoker #'invoke)))
    (run dispatcher flow)))


(defun dispatch-immediately (task invariant &key &allow-other-keys)
  (declare (ignore invariant))
  (funcall task))


(defun run-it-immediately (flow)
  (run #'dispatch-immediately flow))
