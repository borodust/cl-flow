(cl:in-package :cl-flow)

(defun run (dispatcher flow)
  "Dispatcher must be a function with lambda-list congruent to (task arg
invariant &key &allow-other-keys)"
  (declare (type (function ((function () *) t &rest t &key &allow-other-keys) *)
                 dispatcher)
           (type (or list function) flow)
           #.+optimize-form+)
  (let ((context (make-flow-context :native-dispatcher dispatcher)))
    (init-context-dispatcher context)
    (dispatch-serially context (ensure-list flow))))
