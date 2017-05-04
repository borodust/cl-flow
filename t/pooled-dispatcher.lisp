(in-package :cl-flow.tests)


(defclass pooled-dispatcher ()
  ((pool :initform (mt:make-thread-pool 4))))


(defmethod initialize-instance :after ((this pooled-dispatcher) &key)
  (with-slots (pool) this
    (mt:open-pool pool)))


(defvar *dispatcher* (make-instance 'pooled-dispatcher))


(defun dispatch (fn invariant &key)
  (declare (ignore invariant))
  (with-slots (pool) *dispatcher*
    (mt:push-to-pool pool fn)))


(defun run-it (flow)
  (run #'dispatch flow))
