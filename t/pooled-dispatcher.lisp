(in-package :cl-flow.tests)


(defclass pooled-dispatcher ()
  ((pool :initform (mt:make-thread-pool 4))))


(defmethod initialize-instance :after ((this pooled-dispatcher) &key)
  (with-slots (pool) this
    (mt:open-pool pool)))


(defvar *dispatcher* (make-instance 'pooled-dispatcher))


(defmethod dispatch ((this pooled-dispatcher) fn &key)
  (with-slots (pool) this
    (mt:push-to-pool pool fn)))


(defun run-it (flow)
  (run *dispatcher* flow))
