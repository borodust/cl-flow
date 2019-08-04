(cl:in-package :cl-flow)


;;;
;;; DYNAMICALLY
;;;
(defun dispatch-dynamically (flow-context body-fu)
  (declare (type (function (*) *) body-fu)
           #.+optimize-form+)
  (push-flow-stack flow-context (funcall body-fu (flow-context-value flow-context)))
  (continue-dispatch flow-context))


(defmacro dynamically (lambda-list &body body)
  "Generates new flow dynamically during parent flow execution. In other words,
injects new dynamically created flow into a current one. Non-consing."
  (flow-lambda-macro (flow-context)
    (with-flow-function-macro (body-fu lambda-list body)
      `(dispatch-dynamically ,flow-context #',body-fu))))


(defmacro ->> (lambda-list &body body)
  "See flow:dynamically"
  `(dynamically ,lambda-list
     ,@body))
