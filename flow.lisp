(in-package :cl-flow)


(defmacro *> (invariant-n-opts condition-var &body body)
  (declare (ignore invariant-n-opts condition-var body))
  (error "*> cannot be used outside of flow operator"))


(defun nop (result error-p)
  (declare (ignore result error-p)))


(defun expand-body-function-def (name lambda-list body)
  (let* ((destructuring-ll (car lambda-list))
         (destructured-p (and destructuring-ll (listp destructuring-ll)))
         (arg (gensym)))
    `(,name
      (,arg)
      (declare (ignorable ,arg))
      ,(if destructured-p
           `(destructuring-bind ,destructuring-ll ,arg
              ,@body)
           `(,@(if lambda-list
                   `(let ((,(car lambda-list) ,arg)))
                   `(progn))
               ,@body)))))


(defmacro atomically (invariant-n-opts lambda-list &body body)
  (destructuring-bind (&optional invariant &rest opts) (ensure-list invariant-n-opts)
    (with-gensyms (dispatcher arg result-callback return-error dispatched e body-fn)
      `(lambda (,dispatcher ,result-callback ,arg)
         (declare (ignorable ,arg))
         (labels (,(expand-body-function-def body-fn lambda-list body)
                  (,return-error (,e)
                    (funcall ,result-callback ,e t))
                  (,dispatched ()
                    (handler-bind ((simple-error #',return-error))
                      (funcall ,result-callback (funcall #',body-fn ,arg) nil))))
           (funcall ,dispatcher #',dispatched ,invariant ,@opts))))))


(defmacro -> (invariant-n-opts lambda-list &body body)
  `(atomically ,invariant-n-opts ,lambda-list
     ,@body))


(defmacro dynamically (lambda-list &body body)
  (with-gensyms (dispatcher body-fn arg result-callback return-error e)
    `(lambda (,dispatcher ,result-callback ,arg)
       (declare (ignorable ,arg))
       (flet (,(expand-body-function-def body-fn lambda-list body)
              (,return-error (,e)
                (funcall ,result-callback ,e t)))
         (handler-bind ((simple-error #',return-error))
           (funcall (funcall #',body-fn ,arg) ,dispatcher ,result-callback ,arg))))))


(defmacro ->> (lambda-list &body body)
  `(dynamically ,lambda-list
     ,@body))


(defun continue-flow (result)
  (declare (ignore result))
  (error "function can be called inside asynchonous block only"))


(defun interrupt-flow (condition)
  (declare (ignore condition))
  (error "function can be called inside asynchonous block only"))


(defmacro asynchronously (lambda-list &body body)
  (with-gensyms (dispatcher body-fn arg result-callback continue-arg condi)
    `(lambda (,dispatcher ,result-callback ,arg)
       (declare (ignorable ,arg)
                (ignore ,dispatcher))
       (labels ((continue-flow (,continue-arg)
                  (funcall ,result-callback ,continue-arg nil))
                (interrupt-flow (,condi)
                  (funcall ,result-callback ,condi t))
                ,(expand-body-function-def body-fn lambda-list body))
         (handler-bind ((simple-error #'interrupt-flow))
           (funcall #',body-fn ,arg))))))


(defmacro %> (lambda-list &body body)
  `(asynchronously ,lambda-list
     ,@body))


(defun dispatch-serial-flow (list dispatcher result-callback arg)
  (labels ((dispatch-list (fn-list arg)
             (if (null fn-list)
                 (funcall result-callback arg nil)
                 (let ((flow-element (first fn-list)))
                   (flet ((dispatch-next (result error-p)
                            (if error-p
                                (error "Error during serial flow dispatch: ~A" result)
                                (dispatch-list (rest fn-list) result))))
                     (if (listp flow-element)
                         (dispatch-serial-flow flow-element dispatcher #'dispatch-next arg)
                         (funcall flow-element dispatcher #'dispatch-next arg)))))))
    (dispatch-list list arg)))


(defun dispatch-parallel-flow (list dispatcher result-callback arg)
  (if (null list)
      (funcall result-callback nil nil)
      (let ((counter)
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
                            (when (= (decrement-counter counter) 1)
                              (funcall result-callback flow-result nil))))
                     (let ((element (car callback-list)))
                       (cond
                         ((null element) (%cons-result-callback nil nil))
                         ((listp element) (resolve element))
                         (t (funcall element dispatcher #'%cons-result-callback arg))))
                     (when-let ((rest-elements (cdr callback-list)))
                       (resolve rest-elements)))))
          (setf counter (make-atomic-counter (count-elements list)))
          (resolve flow-result)))))


(defmacro serially (&body flow)
  (with-gensyms (dispatcher result-callback arg flow-tree)
    `(lambda (,dispatcher ,result-callback ,arg)
       (declare (type (or null (function (list t) *)) ,result-callback))
       (let ((,flow-tree (list ,@flow)))
         (dispatch-serial-flow ,flow-tree ,dispatcher (or ,result-callback #'nop) ,arg)))))


(defmacro >> (&body flow)
  `(serially ,@flow))


(defmacro concurrently (&body body)
  (with-gensyms (dispatcher arg result-callback flow)
    `(lambda (,dispatcher ,result-callback ,arg)
       (declare (type (or (function (list t) *) null) ,result-callback))
       (let ((,flow (list ,@body)))
         (dispatch-parallel-flow ,flow ,dispatcher (or ,result-callback #'nop) ,arg)))))


(defmacro ~> (&body body)
  `(concurrently ,@body))


(defun run (dispatcher flow)
  "Dispatcher must be a function with lambda-list congruent to (task invariant &key
&allow-other-keys)"
  (dispatch-serial-flow (ensure-list flow) dispatcher #'nop nil))
