(cl:in-package :cl-flow)

(declaim (special *flow-value*))

;;;
;;; REPEATEDLY
;;;
(defun dispatch-repeatedly (flow-context test-fu flow)
  (declare (type (function () boolean) test-fu)
           (type flow-context flow-context)
           #.+optimize-form+)
  (let (extended-flow)
    (flet ((%push-extended-flow (flow-context)
             (let ((*flow-value* (flow-context-value flow-context)))
               (when (funcall test-fu)
                 (push-flow-stack flow-context extended-flow)))
             (continue-dispatch flow-context)))
      (setf extended-flow (append flow (list #'%push-extended-flow)))
      (%push-extended-flow flow-context))))


(defmacro repeatedly (live-test-form &body flow)
  "Short-circuit the flow specified inside the block and executes it repeatedly
in loop until LIVE-TEST-FORM evaluates to NIL. Result from the last iteration
will be passed to the next block"
  (with-gensyms (test-fu)
    (flow-lambda-macro (flow-context)
      `(flet ((,test-fu () ,live-test-form))
         (dispatch-repeatedly ,flow-context #',test-fu (list ,@flow))))))


(defmacro o> (condition &body body)
  "See flow:repeatedly"
  `(repeatedly ,condition
     ,@body))
