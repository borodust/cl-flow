(cl:in-package :cl-user)


(defpackage :cl-flow
  (:nicknames :flow)
  (:use :cl :alexandria)
  (:export #:->
           #:>>
           #:~>
           #:->>
           #:atomically
           #:serially
           #:concurrently
           #:dynamically
           #:define-flow
           #:run))
