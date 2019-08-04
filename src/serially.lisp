(cl:in-package :cl-flow)

;;;
;;; SERIALLY
;;;
(defun dispatch-serially (flow-context flow)
  (declare (type flow-context flow-context)
           #.+optimize-form+)
  (push-flow-stack flow-context flow)
  (dispatch-rest flow-context))


(defmacro serially (&body flow)
  "Executes child elements serially (but possibly in different threads)
returning a value of the last atomic block or flow. Non-consing."
  (flow-lambda-macro (flow-context)
    `(dispatch-serially ,flow-context (list ,@flow))))


(defmacro >> (&rest flow)
  "See flow:serially"
  `(serially ,@flow))
