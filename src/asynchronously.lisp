(cl:in-package :cl-flow)

;;;
;;; ASYNCHRONOUSLY
;;;
(defun dispatch-asynchronously (flow-context body-fu)
  (funcall body-fu (flow-context-value flow-context)))


(defun continue-flow (&optional value)
  "Invokes next flow block with provided value as an argument"
  (declare (ignore value))
  (error "Function can be called inside asynchonous block only"))


(defun interrupt-flow (&optional condition)
  "Interrupts flow with provided condition"
  (declare (ignore condition))
  (error "Function can be called inside asynchonous block only"))


(defmacro asynchronously (lambda-list &body body)
  "Splits current flow allowing manually managing its execution via #'continue-flow and
#'interrupt-flow functions"
  (with-gensyms (continue-arg condi)
    (flow-lambda-macro (flow-context)
      (let ((fu-body `((flet ((continue-flow (&optional ,continue-arg)
                                (capture-flow-value ,flow-context ,continue-arg)
                                (continue-dispatch ,flow-context))
                              (interrupt-flow (&optional ,condi)
                                (error ,condi)))
                         (declare (ignorable (function continue-flow)
                                             (function interrupt-flow)))
                         ,@body))))
        (with-flow-function-macro (body-fu lambda-list fu-body)
          `(dispatch-asynchronously ,flow-context #',body-fu))))))


(defmacro %> (lambda-list &body body)
  "See flow:asynchronously"
  `(asynchronously ,lambda-list
     ,@body))
