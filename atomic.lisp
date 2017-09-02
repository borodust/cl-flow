(in-package :cl-flow)


;; https://github.com/zerth/cl-cas-queue/blob/master/cas-queue.lisp
(defmacro %atomic-decf (place)
  "Atomically decrement the value of PLACE.  For CCL, SBCL, and LW, it
should be an accessor form for a struct slot holding an integer."
  #+lispworks `(sys:atomic-decf ,place)
  #+ccl `(ccl::atomic-decf ,place)
  #+sbcl `(1- (sb-ext:atomic-decf ,place))
  #-(or lispworks ccl sbcl) (error "ATOMIC-DECF unimplemented"))


(defstruct (atomic-counter
             (:constructor %make-atomic-counter (value))
             #+ccl (:type vector))
  (value 0 :type (unsigned-byte 64)))


(defun make-atomic-counter (&optional (value 0))
  (%make-atomic-counter value))


(defun decrement-counter (counter)
  "Returns previous value"
  (%atomic-decf (atomic-counter-value counter)))
