(cl:in-package :cl-user)

(defpackage :cl-flow.def
  (:use :cl :asdf))

(in-package :cl-flow.def)


(defsystem cl-flow
  :description "Data-flow driven concurrency model for Common Lisp"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria)
  :serial t
  :components ((:file "packages")
               (:file "atomic")
               (:file "flow")))


(defsystem cl-flow/tests
  :description "Test suite for cl-flow"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-flow fiveam cl-muth simple-flow-dispatcher)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "pooled-dispatcher")
               (:file "suite")))
