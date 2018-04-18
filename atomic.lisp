(cl:in-package :cl-flow)


;; https://github.com/zerth/cl-cas-queue/blob/master/cas-queue.lisp
(defmacro %atomic-decf (place)
  "Atomically decrement the value of PLACE.  For CCL, SBCL, and LW, it
should be an accessor form for a struct slot holding an integer."
  #+lispworks `(sys:atomic-decf ,place)
  #+ccl `(ccl::atomic-decf ,place)
  #+sbcl `(1- (sb-ext:atomic-decf ,place))
  #-(or lispworks ccl sbcl) (error "ATOMIC-DECF unimplemented"))


#-ccl
(defstruct (atomic-counter
             (:constructor %make-atomic-counter (value)))
  (value 0 :type (unsigned-byte 64)))


#+ccl
(progn
  (deftype atomic-counter () '(simple-array t 1))
  (defun* (%make-atomic-counter -> atomic-counter) ((value (unsigned-byte 64)))
    (make-array 1 :element-type t :initial-element value))

  (defmacro atomic-counter-value (counter)
    `(svref ,counter 0)))


(defun* (make-atomic-counter -> atomic-counter)
    (&optional ((value (unsigned-byte 64)) 0))
  (%make-atomic-counter value))


(defun* (decrement-counter -> (unsigned-byte 64)) ((counter atomic-counter))
  "Returns previous value"
  (%atomic-decf (atomic-counter-value counter)))
