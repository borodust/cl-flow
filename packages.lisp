(cl:defpackage :cl-flow
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

           #:rerun-flow-block
           #:skip-flow-block
           #:use-flow-block-value))
