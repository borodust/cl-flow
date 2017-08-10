(cl:in-package :cl-user)


(defpackage :cl-flow
  (:nicknames :flow)
  (:use :cl :alexandria)
  (:export #:->
           #:>>
           #:~>
           #:->>
           #:%>
           #:atomically
           #:serially
           #:concurrently
           #:dynamically
           #:asynchronously
           #:continue-flow
           #:interrupt-flow
           #:run
           #:skip-flow-block))
