(cl:in-package :cl-user)


(defpackage :cl-flow
  (:use :cl :alexandria)
  (:export #:dispatch
           #:->
           #:>>
           #:~>
           #:define-flow
           #:run-flow))
