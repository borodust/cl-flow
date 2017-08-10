(in-package :cl-flow.tests)


(define-condition skipping-condition (simple-error) ())

(define-condition recoverable-condition (simple-error) ())

(defclass pooled-dispatcher ()
  ((pool :initform (mt:make-thread-pool 4))))


(defmethod initialize-instance :after ((this pooled-dispatcher) &key)
  (with-slots (pool) this
    (mt:open-pool pool)))


(defvar *dispatcher* (make-instance 'pooled-dispatcher))


(defun dispatch (fn invariant &key)
  (declare (ignore invariant))
  (with-slots (pool) *dispatcher*
    (flet ((wrapped ()
             (handler-bind ((skipping-condition #'skip-flow-block)
                            (recoverable-condition (lambda (e)
                                                     (declare (ignore e))
                                                     (use-value -1))))
               (funcall fn))))
      (mt:push-to-pool pool #'wrapped))))


(defun run-it (flow)
  (run #'dispatch flow))
