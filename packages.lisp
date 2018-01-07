(cl:in-package :cl-user)


(defpackage :cl-flow
  (:nicknames :flow)
  (:use :cl :alexandria :defstar)
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
           #:run))
