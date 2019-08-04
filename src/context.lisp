(cl:in-package :cl-flow)

(defvar *context* nil)
(defvar *continue* nil)


(defstruct flow-context
  (native-dispatcher nil
   :type (function (function * &rest * &key &allow-other-keys) *)
   :read-only t)
  (dispatcher nil
   :type (or null (function (* &rest * &key &allow-other-keys))))
  (value nil
   :type t)
  (function nil
   :type (or null (function (*) *)))
  (lambda nil
    :type (or null (function (flow-context) *)))
  (stack (make-array 3 :element-type 'list :initial-element nil
                       :fill-pointer 0 :adjustable t)
   :type array
   :read-only t))


(defun dispatch (context task invariant &rest args &key &allow-other-keys)
  (setf (flow-context-function context) task)
  (apply (flow-context-dispatcher context) invariant args))


(defun continue-dispatch (context)
  (if (eq *context* context)
      (setf *continue* t)
      (dispatch-rest context)))


(defun make-child-flow-context (parent-context)
  (init-context-dispatcher
   (make-flow-context :native-dispatcher (flow-context-native-dispatcher parent-context)
                      :value (flow-context-value parent-context))))


(defun dispatch-rest (flow-context)
  (declare (type flow-context flow-context)
           #.+optimize-form+)
  (let ((*context* flow-context)
        (*continue* nil))
    (loop for block = (chop-head flow-context)
          do (setf *continue* nil)
             (when block
               (if (listp block)
                   (progn
                     (push-flow-stack flow-context block)
                     (setf *continue* t))
                   (funcall block flow-context)))
          while *continue*)))


(defun capture-flow-value (context value)
  (setf (flow-context-value context) value))


(defun push-flow-stack (context flow)
  (vector-push-extend (ensure-list flow) (flow-context-stack context)))


(defun chop-head (context)
  (let ((stack (flow-context-stack context)))
    (symbol-macrolet ((current (aref stack (1- (length stack)))))
      (flet ((%chop-head ()
               (let* ((top current)
                      (head (first top))
                      (tail (rest top)))
                 (prog1 head
                   (if tail
                       (setf current tail)
                       (vector-pop stack))))))
        (loop while (> (length stack) 0)
                thereis (%chop-head))))))


;;;
;;; RESTARTS
;;;
(defun try-restart (restart-name &optional (arg nil provided-p))
  (if provided-p
      (invoke-restart restart-name arg)
      (invoke-restart restart-name)))


(defun rerun-flow-block ()
  (try-restart 'rerun-flow-block))


(defun skip-flow-block ()
  (try-restart 'skip-flow-block))


(defun inject-flow (flow)
  (try-restart 'inject-flow flow))


(defun use-flow-block-value (value)
  (try-restart 'use-flow-block-value value))


(defun invoke-with-restarts (flow-context fu arg)
  (declare (type (function (*) *) fu)
           #.+optimize-form+)
  (let (result)
    (tagbody restart-block begin
       (restart-case
           (setf result (funcall fu arg))
         (rerun-flow-block ()
           :report "Rerun current flow block"
           (go begin))
         (skip-flow-block ()
           :report "Skip flow block returning nil")
         (use-flow-block-value (value)
           :report "Skip flow block returning provided value"
           (setf result value))
         (inject-flow (flow)
           :report "Inject flow to run instead of current block"
           (push-flow-stack flow-context flow)
           (continue-dispatch flow-context))))
    result))


(defun invoke-flow-function (context)
  (capture-flow-value context (invoke-with-restarts context
                                                    (flow-context-function context)
                                                    (flow-context-value context)))
  (continue-dispatch context))


(defun init-context-dispatcher (context)
  (let ((dispatcher (flow-context-native-dispatcher context)))
    (labels ((%invoke ()
               (invoke-flow-function context))
             (%dispatcher (invariant &rest args &key &allow-other-keys)
               (apply dispatcher #'%invoke invariant args)))
      (setf (flow-context-dispatcher context) #'%dispatcher)))
  context)


(defun run (dispatcher flow)
  "Dispatcher must be a function with lambda-list congruent to (task arg
invariant &key &allow-other-keys)"
  (declare (type (function ((function () *) * &rest * &key &allow-other-keys) *)
                 dispatcher)
           (type (or list function) flow)
           #.+optimize-form+)
  (let ((context (make-flow-context :native-dispatcher dispatcher)))
    (init-context-dispatcher context)
    (dispatch-serially context (ensure-list flow))))
