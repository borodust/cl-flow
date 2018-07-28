(cl:in-package :cl-flow)


#+flow-optimized
(alexandria:define-constant +optimize-form+ '(optimize (speed 3) (safety 0) (debug 0))
  :test #'equal)
#-flow-optimized
(alexandria:define-constant +optimize-form+ '(optimize) :test #'equal)
