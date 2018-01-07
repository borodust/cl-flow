(in-package :cl-flow)


(alexandria:define-constant +optimize-form+ '(optimize (speed 3) (safety 0) (debug 0))
  :test #'equal)
