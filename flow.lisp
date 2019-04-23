(cl:in-package :cl-flow)


(declaim (special *flow-value*))

(defvar *recursive-flow* nil)

(defun nop (result error-p)
  (declare (ignore result error-p)
           #.+optimize-form+))


(defmacro flow-lambda ((dispatcher result-callback arg) &body body)
  `(lambda (,dispatcher ,result-callback ,arg)
     (declare (type (function (function &rest *)) ,dispatcher)
              (type (or null (function (* boolean))) ,result-callback)
              (ignorable ,arg)
              #.+optimize-form+)
     ,@body))


(defun invoke-with-restarts (fu arg result-callback dispatcher)
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
           (invoke-dynamically (constantly flow) arg result-callback dispatcher))))
    result))


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


(defmacro with-body-fu ((fu-name lambda-list fu-body) &body body)
  (let* ((destructuring-ll (car lambda-list))
         (destructured-p (and destructuring-ll (listp destructuring-ll)))
         (arg (gensym)))
    (unless (or destructured-p (null (cdr lambda-list)))
      (error "Flow block can accept single argument only, but got ~A" lambda-list))
    `(flet ((,fu-name (,arg)
              (declare (ignorable ,arg))
              (,@(cond
                   (destructured-p
                    `(destructuring-bind ,destructuring-ll ,arg))
                   ((not (null lambda-list))
                    `(let ((,(car lambda-list) ,arg))))
                   (t '(progn)))
               ,@fu-body)))
       ,@body)))


(defun invoke-atomically (body-fu arg result-callback dispatcher invariant &rest opts)
  (declare (type (function (*) *) body-fu)
           (type (function (* boolean)) result-callback)
           (type (function (* &rest *)) dispatcher)
           #.+optimize-form+)
  (labels ((return-error (e)
             (funcall result-callback e t))
           (dispatched ()
             (handler-bind ((simple-error #'return-error))
               (funcall result-callback
                        (invoke-with-restarts body-fu
                                              arg
                                              result-callback
                                              dispatcher)
                        nil))))
    (apply dispatcher #'dispatched invariant opts)))


(defun parse-atomic-block-args (args)
  (loop for (opt . body) on args by #'cddr
        until (listp opt)
        collecting opt into opts
        collecting (first body) into opts
        finally (return (list opts opt body))))


(defmacro atomically (invariant &body args)
  "Encloses atomic flow block of code that could be dispatched concurrently"
  (destructuring-bind (opts lambda-list body) (parse-atomic-block-args args)
    (with-gensyms (dispatcher arg result-callback body-fu)
      `(flow-lambda (,dispatcher ,result-callback ,arg)
         (declare (ignorable ,arg))
         (with-body-fu (,body-fu ,lambda-list ,body)
           (invoke-atomically #',body-fu ,arg
                              ,result-callback
                              ,dispatcher ,invariant ,@opts))))))


(defmacro -> (invariant &body args)
  "See flow:atomically"
  `(atomically ,invariant ,@args))


(defun invoke-dynamically (body-fu arg result-callback dispatcher)
  (declare (type (function (*) *) body-fu)
           (type (function (* boolean)) result-callback)
           (type (function (* &rest *)) dispatcher )
           #.+optimize-form+)
  (flet ((return-error (e)
           (funcall result-callback e t)))
    (handler-bind ((simple-error #'return-error))
      (let ((flow-element (invoke-with-restarts body-fu arg result-callback dispatcher)))
        (if (listp flow-element)
            (dispatch-serial-flow flow-element dispatcher result-callback arg)
            (funcall (the (function (function function *)) flow-element)
                     dispatcher result-callback arg))))))


(defmacro dynamically (lambda-list &body body)
  "Generates new flow dynamically during parent flow execution. In other words, injects new
dynamically created flow into a current one."
  (with-gensyms (dispatcher body-fu arg result-callback)
    `(flow-lambda (,dispatcher ,result-callback ,arg)
       (declare (ignorable ,arg))
       (with-body-fu (,body-fu ,lambda-list ,body)
         (invoke-dynamically #',body-fu ,arg ,result-callback ,dispatcher)))))


(defmacro ->> (lambda-list &body body)
  "See flow:dynamically"
  `(dynamically ,lambda-list
     ,@body))



(defun %invoke-repeatedly (dispatcher test-fu flow arg result-callback)
  (declare (type (function () boolean) test-fu)
           (type (function (* boolean)) result-callback)
           (type (function (* &rest *)) dispatcher)
           #.+optimize-form+)
  (let ((arg arg))
    (tagbody start
       (labels ((return-error (e)
                  (funcall result-callback e t))
                (next-iteration (result error-p)
                  (when error-p
                    (error result))
                  (if (let ((*flow-value* result))
                        (funcall test-fu))
                      (if *recursive-flow*
                          (progn
                            (setf arg result)
                            (go start))
                          (%invoke-repeatedly dispatcher
                                              test-fu
                                              flow
                                              result
                                              result-callback))
                      (funcall result-callback result error-p))))
         (handler-bind ((simple-error #'return-error))
           (let ((*recursive-flow* flow))
             (dispatch-serial-flow flow dispatcher #'next-iteration arg)))))))


(defun invoke-repeatedly (dispatcher test-fu flow arg result-callback)
  (declare (type (function () boolean) test-fu)
           (type (function (* boolean)) result-callback)
           (type (function (* &rest *)) dispatcher)
           #.+optimize-form+)
  (if (let ((*flow-value* arg))
        (funcall test-fu))
      (%invoke-repeatedly dispatcher test-fu flow arg result-callback)
      (funcall result-callback arg nil)))


(defmacro repeatedly (end-test-form &body flow)
  ""
  (with-gensyms (dispatcher test-fu arg result-callback flow-tree)
    `(flow-lambda (,dispatcher ,result-callback ,arg)
       (flet ((,test-fu ()
                ,end-test-form))
         (let ((,flow-tree (list ,@flow)))
           (invoke-repeatedly ,dispatcher
                              #',test-fu
                              ,flow-tree
                              ,arg
                              ,result-callback))))))


(defmacro o> (condition &body body)
  "See flow:repeatedly"
  `(repeatedly ,condition
     ,@body))


(defun continue-flow (&optional value)
  "Invokes next flow block with provided value as an argument"
  (declare (ignore value))
  (error "Function can be called inside asynchonous block only"))


(defun interrupt-flow (condition)
  "Interrupts flow with provided condition"
  (declare (ignore condition))
  (error "Function can be called inside asynchonous block only"))


(defmacro asynchronously (lambda-list &body body)
  "Splits current flow allowing manually managing its execution via #'continue-flow and
#'interrupt-flow functions"
  (with-gensyms (dispatcher body-fu arg result-callback continue-arg condi)
    `(flow-lambda (,dispatcher ,result-callback ,arg)
       (declare (ignorable ,arg))
       (with-body-fu (,body-fu ,lambda-list
                               ((flet ((flow:continue-flow (&optional ,continue-arg)
                                         (funcall ,result-callback ,continue-arg nil))
                                       (flow:interrupt-flow (,condi)
                                         (funcall ,result-callback ,condi t)))
                                  (declare (ignorable (function continue-flow)
                                                      (function interrupt-flow)))
                                  ,@body)))
         (handler-bind ((simple-error #'interrupt-flow))
           (invoke-with-restarts #',body-fu ,arg ,result-callback ,dispatcher))))))


(defmacro %> (lambda-list &body body)
  "See flow:asynchronously"
  `(asynchronously ,lambda-list
     ,@body))


(defun dispatch-serial-flow (list dispatcher result-callback arg)
  (declare (type list list)
           (type (function (* &rest *)) dispatcher)
           (type (function (* boolean)) result-callback)
           #.+optimize-form+)
  (if (null list)
      (funcall result-callback arg nil)
      (let ((flow-element (first list)))
        (flet ((dispatch-next (result error-p)
                 (if error-p
                     (error result)
                     (dispatch-serial-flow (rest list)
                                           dispatcher
                                           result-callback
                                           result))))
          (if (listp flow-element)
              (dispatch-serial-flow flow-element dispatcher #'dispatch-next arg)
              (funcall (the (function (function function *)) flow-element)
                       dispatcher #'dispatch-next arg))))))


(defun dispatch-parallel-flow (list dispatcher result-callback arg)
  (declare (type list list)
           (type (function (* &rest *)) dispatcher)
           (type (function (* boolean)) result-callback)
           #.+optimize-form+)
  (if (null list)
      (funcall result-callback nil nil)
      (labels ((count-elements (root)
                 (the fixnum (if (and root (listp root))
                                 (loop for node in root summing (count-elements node) fixnum)
                                 1))))
        (let ((counter (mt:make-atomic-counter (count-elements list)))
              (flow-result (copy-tree list)))
          (labels ((resolve (callback-list)
                     (flet ((%cons-result-callback (result error-p)
                              (when error-p
                                (error result))
                              (setf (car callback-list) result)
                              (when (= (the fixnum (mt:decrement-atomic-counter counter)) 0)
                                (funcall result-callback flow-result nil))))
                       (let ((element (car callback-list)))
                         (cond
                           ((null element) (%cons-result-callback nil nil))
                           ((listp element) (resolve element))
                           (t (funcall (the (function (function function *)) element)
                                       dispatcher #'%cons-result-callback arg))))
                       (when-let ((rest-elements (cdr callback-list)))
                         (resolve rest-elements)))))
            (resolve flow-result))))))


(defmacro serially (&rest flow)
  "Executes child elements serially (but possibly in different threads) returning a value of the
last atomic block or flow"
  (with-gensyms (dispatcher result-callback arg flow-tree)
    `(let ((,flow-tree (list ,@flow)))
       (flow-lambda (,dispatcher ,result-callback ,arg)
         (declare (type (or null (function (list t) *)) ,result-callback))
         (dispatch-serial-flow ,flow-tree ,dispatcher (or ,result-callback #'nop) ,arg)))))


(defmacro >> (&rest flow)
  "See flow:serially"
  `(serially ,@flow))


(defmacro concurrently (&rest body)
  "Executes child elements in parallel, returning a list of results for child blocks or flows in
the same order they were specified"
  (with-gensyms (dispatcher arg result-callback flow)
    `(let ((,flow (list ,@body)))
       (flow-lambda (,dispatcher ,result-callback ,arg)
         (dispatch-parallel-flow ,flow ,dispatcher (or ,result-callback #'nop) ,arg)))))


(defmacro ~> (&rest body)
  "See flow:concurrently"
  `(concurrently ,@body))


(defun run (dispatcher flow)
  "Dispatcher must be a function with lambda-list congruent to (task invariant &key
&allow-other-keys)"
  (declare (type (function (function &rest *)) dispatcher)
           (type (or list function) flow)
           #.+optimize-form+)
  (dispatch-serial-flow (ensure-list flow) dispatcher #'nop nil))
