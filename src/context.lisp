(cl:in-package :cl-flow)

(defvar *current-context* nil)
(defvar *continue* nil)
(defvar *parent-context* nil)

(define-constant +min-stack-extension+ 3)

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
  (stack (make-array +min-stack-extension+ :element-type 'list :initial-element nil
                                           :fill-pointer 0 :adjustable t)
   :type array
   :read-only t)
  (parent nil
   :type (or null flow-context)
   :read-only t))


(defun dispatch (context task invariant &rest args &key &allow-other-keys)
  (setf (flow-context-function context) task)
  (apply (flow-context-dispatcher context) invariant args))


(defun dispatch-rest (context)
  (declare (type flow-context context)
           #.+optimize-form+)
  (cond
    ((eq *current-context* context) (setf *continue* t))
    ((eq *parent-context* context)
     ;; see catch in #'%dispatch-rest:
     ;; this is to unwind a stack
     ;; to avoid overflowing it
     ;; in case of single-threaded dispatch
     (throw *parent-context* t))
    (t (%dispatch-rest context))))


(defun make-child-flow-context (parent-context)
  (init-context-dispatcher
   (make-flow-context :native-dispatcher (flow-context-native-dispatcher parent-context)
                      :value (flow-context-value parent-context)
                      :parent parent-context)))


(defun %dispatch-rest (flow-context)
  (declare (type flow-context flow-context)
           #.+optimize-form+)
  (let ((*parent-context* *current-context*)
        (*current-context* flow-context)
        (*continue* nil))
    (loop for block = (chop-head flow-context)
          do (setf *continue* nil)
             (when block
               (if (listp block)
                   (progn
                     (push-flow-stack flow-context block)
                     (setf *continue* t))
                   (when (catch *current-context*
                           (funcall block flow-context)
                           nil)
                     (setf *continue* t))))
          while *continue*)))


(defun capture-flow-value (context value)
  (setf (flow-context-value context) value))


(defun push-flow-stack (context flow)
  (vector-push-extend flow (flow-context-stack context) +min-stack-extension+))


(defun chop-head (context)
  (let ((stack (flow-context-stack context)))
    (symbol-macrolet ((current (aref stack (1- (length stack)))))
      (flet ((%chop-head ()
               (let ((top current))
                 (if (listp top)
                     (let ((head (first top))
                           (tail (rest top)))
                       (prog1 head
                         (if tail
                             (setf current tail)
                             (vector-pop stack))))
                     (vector-pop stack)))))
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


(defun %%rerun-invoke ()
  (throw 'begin (values nil t nil)))

(defun %%rerun-invoke-text (stream)
  (format stream "Rerun current flow block"))

(defun %%skip-invoke ()
  (throw 'begin (values nil nil nil)))

(defun %%skip-invoke-text (stream)
  (format stream "Skip flow block returning nil"))

(defun %%use-invoke (value)
  (throw 'begin (values value nil nil)))

(defun %%use-invoke-text (stream)
  (format stream "Skip flow block returning provided value"))

(defun %%inject-invoke (flow)
  (throw 'begin (values nil nil flow)))

(defun %%inject-invoke-text (stream)
  (format stream "Inject flow to run instead of current block"))

(defun %invoke-with-restarts (fu arg)
  (declare (type (function (*) *) fu)
           #.+optimize-form+)
  ;; catch+restart-bind insead of tagbody+restart-case to avoid any consing
  (catch 'begin
    (restart-bind ((rerun-flow-block #'%%rerun-invoke
                                     :report-function #'%%rerun-invoke-text)
                   (skip-flow-block #'%%skip-invoke
                                    :report-function #'%%skip-invoke-text)
                   (use-flow-block-value #'%%use-invoke
                                         :report-function #'%%use-invoke-text)
                   (inject-flow #'%%inject-invoke
                                :report-function #'%%inject-invoke-text))
      (values (funcall fu arg) nil nil))))


(defun invoke-with-restarts (flow-context fu arg)
  (declare (type (function (*) *) fu)
           #.+optimize-form+)
  ;; loop instead tagbody to avoid any consing
  (loop do (multiple-value-bind (result looping-p flow)
               (%invoke-with-restarts fu arg)
             (cond
               (flow (push-flow-stack flow-context flow)
                     (dispatch-rest flow-context)
                     (return))
               ((not looping-p) (return result))))))


(defun invoke-flow-function (context)
  (capture-flow-value context (invoke-with-restarts context
                                                    (flow-context-function context)
                                                    (flow-context-value context)))
  (dispatch-rest context))


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
