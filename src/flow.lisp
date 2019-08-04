(cl:in-package :cl-flow)


(defun parse-atomic-block-args (args)
  (loop for (opt . body) on args by #'cddr
        until (listp opt)
        collecting opt into opts
        collecting (first body) into opts
        finally (return (list opts opt body))))


(defmacro %flow-lambda ((flow-context) &body body)
  `(lambda (,flow-context)
     (declare (type flow-context ,flow-context)
              #.+optimize-form+)
     ,@body))


(defmacro flow-lambda-macro ((flow-context) &body body)
  `(with-gensyms (,flow-context)
     `(%flow-lambda (,,flow-context)
        ,,@body)))


(defmacro %with-flow-function ((fu-name fu-lambda-list &body fu-body) &body body)
  (let* ((destructuring-ll (car fu-lambda-list))
         (destructured-p (and destructuring-ll (listp destructuring-ll)))
         (arg (gensym)))
    (unless (or destructured-p (null (cdr fu-lambda-list)))
      (error "Flow block can accept single argument only, but got ~A" fu-lambda-list))
    `(flet ((,fu-name (,arg)
              (declare (ignorable ,arg))
              (,@(cond
                   (destructured-p
                    `(destructuring-bind ,destructuring-ll ,arg))
                   ((not (null fu-lambda-list))
                    `(let ((,destructuring-ll ,arg))))
                   (t '(progn)))
               ,@fu-body)))
       ,@body)))


(defmacro with-flow-function-macro ((fu-name fu-lambda-list fu-body) &body body)
  `(with-gensyms (,fu-name)
     `(%with-flow-function (,,fu-name ,,fu-lambda-list ,@,fu-body)
        ,,@body)))
