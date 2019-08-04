(cl:in-package :cl-flow)


;;;
;;; ATOMICALLY
;;;
(defun dispatch-atomically (context fu invariant
                            &rest opts &key &allow-other-keys)
  (apply #'dispatch context fu invariant opts))


(defmacro atomically (invariant &body args)
  "Encloses atomic flow block of code that can be dispatched concurrently"
  (destructuring-bind (opts lambda-list body) (parse-atomic-block-args args)
    (flow-lambda-macro (flow-context)
      (with-flow-function-macro (body-fu lambda-list body)
        `(dispatch-atomically ,flow-context #',body-fu ,invariant ,@opts)))))


(defmacro -> (invariant &body args)
  "See flow:atomically"
  `(atomically ,invariant ,@args))
