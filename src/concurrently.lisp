(cl:in-package :cl-flow)


;;;
;;; CONCURRENTLY
;;;

(defun %dispatch-concurrently (parent-context flow)
  (declare (type list flow)
           (type flow-context parent-context)
           #.+optimize-form+)
  (labels ((%leaf-count (tree)
             (loop for node in tree
                   if (and node (listp node))
                     summing (%leaf-count node) into result
                   else
                     counting t into result
                   finally (return result))))
    (let* ((results (copy-tree flow))
           (counter (mt:make-atomic-counter (%leaf-count results))))
      (labels ((%countdown ()
                 (when (= (mt:decrement-atomic-counter counter) 0)
                   (capture-flow-value parent-context results)
                   (continue-dispatch parent-context)))
               (%make-context (child-flow cell)
                 (flet ((%capture-result (child-context)
                          (setf (car cell) (flow-context-value child-context))
                          (%countdown)))
                   (let ((context (make-child-flow-context parent-context)))
                     (push-flow-stack context #'%capture-result)
                     (push-flow-stack context child-flow)
                     context)))
               (%dispatch-flow (flow results)
                 (loop for block in flow
                       for cell on results
                       if (listp block)
                         do (if block
                                (%dispatch-flow block (car cell))
                                (progn
                                  (setf (car cell) nil)
                                  (%countdown)))
                       else
                         do (dispatch-rest (%make-context block cell)))))
        (%dispatch-flow flow results)))))


(defun dispatch-concurrently (parent-context flow)
  (declare (type list flow)
           (type flow-context parent-context)
           #.+optimize-form+)
  (if flow
      (%dispatch-concurrently parent-context flow)
      (continue-dispatch parent-context)))


(defmacro concurrently (&body flow)
  "Executes child elements in parallel, returning a list of results for child
blocks or flows in the same order they were specified. Consing."
  (flow-lambda-macro (flow-context)
    `(dispatch-concurrently ,flow-context (list ,@flow))))


(defmacro ~> (&rest body)
  "See flow:concurrently"
  `(concurrently ,@body))
