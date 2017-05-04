(in-package :cl-flow)


(defmacro *> (invariant-n-opts condition-var &body body)
  (declare (ignore invariant-n-opts condition-var body))
  (error "*> cannot be used outside of flow operator"))


(defun nop (result error-p)
  (declare (ignore result error-p)))


(declaim (ftype (function (* (or function null) * list function list) *) invariant-dispatch))
(defun invariant-dispatch (dispatcher result-callback invariant opts fn args)
  (labels ((return-error (e)
             (funcall result-callback (list e) t))
           (dispatched ()
             (handler-bind ((simple-error #'return-error))
               (funcall result-callback
                        (multiple-value-list (apply fn args)) nil))))
    (apply dispatcher #'dispatched invariant opts)))


(defun insert-rest-arg (lambda-list name)
  (multiple-value-bind (required optional rest key)
      (parse-ordinary-lambda-list lambda-list)
    (if rest
        (values lambda-list nil)
        (values (append required
                        (when optional
                          (append (list '&optional) optional))
                        (list '&rest name)
                        (when key
                          (append (list '&key) key)))
                t))))


(defmacro atomically (invariant-n-opts lambda-list &body body)
  (destructuring-bind (&optional invariant &rest opts) (ensure-list invariant-n-opts)
    (with-gensyms (dispatcher body-fn args result-callback rest-arg)
      (multiple-value-bind (new-lambda-list new-rest-p) (insert-rest-arg lambda-list rest-arg)
        `(lambda (,dispatcher ,result-callback &rest ,args)
           (declare (ignorable ,args))
           (flet ((,body-fn ,new-lambda-list
                    ,@(when new-rest-p
                        `((declare (ignore ,rest-arg))))
                    ,@body))
                    (invariant-dispatch ,dispatcher (or ,result-callback #'nop)
                                        ,invariant (list ,@opts)
                                        #',body-fn ,(when (not (null lambda-list)) args))))))))


(defmacro -> (invariant-n-opts lambda-list &body body)
  `(atomically ,invariant-n-opts ,lambda-list
     ,@body))


(defun inject-flow (flow-gen dispatcher result-callback args)
  (flet ((return-error (e)
           (funcall result-callback (list e) t)))
    (handler-bind ((simple-error #'return-error))
      (apply (apply flow-gen args) dispatcher result-callback args))))


(defmacro dynamically (lambda-list &body body)
  (with-gensyms (dispatcher body-fn args result-callback rest-arg)
    (multiple-value-bind (new-lambda-list new-rest-p) (insert-rest-arg lambda-list rest-arg)
      `(lambda (,dispatcher ,result-callback &rest ,args)
         (declare (ignorable ,args))
         (flet ((,body-fn ,new-lambda-list
                  ,@(when new-rest-p
                      `((declare (ignore ,rest-arg))))
                  ,@body))
           (inject-flow #',body-fn ,dispatcher ,result-callback ,args))))))


(defmacro ->> (lambda-list &body body)
  `(dynamically ,lambda-list
     ,@body))


(defun dispatch-serial-flow (list dispatcher result-callback args)
  (labels ((dispatch-list (fn-list args)
             (flet ((dispatch-next (result error-p)
                      (if error-p
                          (error "Error during serial flow dispatch: ~A" result)
                          (dispatch-list (rest fn-list) result))))
               (if (null fn-list)
                   (funcall result-callback args nil)
                   (let ((flow-element (first fn-list)))
                     (if (listp flow-element)
                         (dispatch-serial-flow flow-element dispatcher #'dispatch-next args)
                         (apply flow-element dispatcher #'dispatch-next args)))))))
    (dispatch-list list args)))


(defun dispatch-parallel-flow (list dispatcher result-callback args)
  (if (null list)
      (funcall result-callback nil nil)
      (let ((n 0)
            (lock (bt:make-lock "~>"))
            (flow-result (copy-tree list)))
        (labels ((count-elements (root)
                   (if (and root (listp root))
                       (loop for node in root summing (count-elements node))
                      1))
                 (resolve (callback-list)
                   (flet ((%cons-result-callback (result error-p)
                            (when error-p
                              (error "Error during parralel flow dispatch: ~A"
                                     result))
                            (setf (car callback-list) result)
                            (when (= (bt:with-lock-held (lock) (decf n)) 0)
                              (funcall result-callback flow-result nil))))
                     (let ((element (car callback-list)))
                       (cond
                         ((null element) (%cons-result-callback nil nil))
                         ((listp element) (resolve element))
                         (t (apply element dispatcher #'%cons-result-callback args))))
                     (when-let ((rest-elements (cdr callback-list)))
                       (resolve rest-elements)))))
          (setf n (count-elements list))
          (resolve flow-result)))))


(defmacro serially (&body flow)
  (with-gensyms (dispatcher result-callback args flow-tree)
    `(lambda (,dispatcher ,result-callback &rest ,args)
       (declare (type (or null (function (list t) *)) ,result-callback))
       (let ((,flow-tree (list ,@flow)))
         (dispatch-serial-flow ,flow-tree ,dispatcher (or ,result-callback #'nop) ,args)))))


(defmacro >> (&body flow)
  `(serially ,@flow))


(defmacro concurrently (&body body)
  (with-gensyms (dispatcher args result-callback flow)
    `(lambda (,dispatcher ,result-callback &rest ,args)
       (declare (type (or (function (list t) *) null) ,result-callback))
       (let ((,flow (list ,@body)))
         (dispatch-parallel-flow ,flow ,dispatcher (or ,result-callback #'nop) ,args)))))


(defmacro ~> (&body body)
  `(concurrently ,@body))


(defun run (dispatcher flow)
  "Dispatcher must be a function with lambda-list congruent to (task invariant &key
&allow-other-keys)"
  (dispatch-serial-flow (ensure-list flow) dispatcher #'nop nil))
