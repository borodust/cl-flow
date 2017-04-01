(cl:in-package :cl-user)


(defpackage :cl-flow
  (:nicknames :flow)
  (:use :cl :alexandria)
  (:export #:dispatch
           #:->
           #:>>
           #:~>
           #:->>
           #:atomically
           #:serially
           #:concurrently
           #:dynamically
           #:define-flow
           #:run))
