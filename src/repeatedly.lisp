(cl:in-package :cl-flow)

(declaim (special *flow-value*))

;;;
;;; REPEATEDLY
;;;
(defun dispatch-repeatedly (flow-context test-fu flow)
  (declare (type (function () boolean) test-fu)
           (type flow-context flow-context)
           #.+optimize-form+)
  (labels ((%push-repeated-flow (flow-context)
             (when (let ((*flow-value* (flow-context-value flow-context)))
                     (funcall test-fu))
               (push-flow-stack flow-context #'%push-repeated-flow)
               (push-flow-stack flow-context flow))
             (dispatch-rest flow-context)))
    (%push-repeated-flow flow-context)))


(defmacro repeatedly (live-test-form &body flow)
  "Short-circuit the flow specified inside the block and executes it repeatedly
in loop until LIVE-TEST-FORM evaluates to NIL. Result from the last iteration
will be passed to the next block. Non-consing."
  (with-gensyms (test-fu)
    (with-flow-let-macro (flow-list flow)
      (flow-lambda-macro (flow-context)
        `(flet ((,test-fu () ,live-test-form))
           (dispatch-repeatedly ,flow-context #',test-fu ,flow-list))))))


(defmacro o> (condition &body body)
  "See flow:repeatedly"
  `(repeatedly ,condition
     ,@body))
