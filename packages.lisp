(cl:defpackage :cl-flow
  (:nicknames :flow)
  (:use :cl :alexandria)
  (:export #:->
           #:>>
           #:~>
           #:->>
           #:%>
           #:o>

           #:atomically
           #:serially
           #:concurrently
           #:dynamically
           #:repeatedly
           #:*flow-value*
           #:asynchronously
           #:continue-flow
           #:interrupt-flow
           #:run

           #:rerun-flow-block
           #:skip-flow-block
           #:use-flow-block-value
           #:inject-flow))
