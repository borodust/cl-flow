(in-package :cl-flow.tests)

(5am:def-suite :cl-flow-suite)

(5am:in-suite :cl-flow-suite)


(defun serial-flow ()
  (>> (loop repeat 5
         collecting (-> :p (a)
                      (1+ a)))))


(defun parallel-flow ()
  (~> (loop for i from 0 below 3
         collecting (let ((i i))
                      (-> :p (a)
                        (+ a i))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test simple-example
  (let ((result ""))
    (mt:wait-with-latch (latch)
      (run-it
       (>> (~> (flow:atomically :tag-0 () "Hello")
               (-> :tag-1 () ", concurrent"))
           (-> :tag-2 ((a b))
             (concatenate 'string a b " World!"))
           (-> :tag-3 (text)
             (setf result text)
             (mt:open-latch latch)))))
    (5am:is (equal "Hello, concurrent World!" result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test basic-flow
  (let ((result ""))
    (mt:wait-with-latch (latch)
      (run-it
       (>> (-> :p () 1)
           (-> :p (a) (+ a 1))
           (-> :p (v)
             (setf result v)
             (mt:open-latch latch)))))
    (5am:is (equal 2 result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test complex-flow
  (let ((result (list)))
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (-> :g ()
               (put 0)
               1)
             (~> (-> :g (a)
                   (+ 1 a))
                 (list nil)
                 (-> :g (a)
                   (+ a 2))
                 (>> (-> :g (b)
                       (+ b 6))
                     (-> :g (b)
                       (+ b 7)))
                 (list (-> :g (a)
                         (+ a 3))
                       (-> :g (a)
                         (+ a 4))
                       (-> :g (a)
                         (values (+ a 5) -1))))
             (-> :g ((a (n) b c (d e f)))
               (put (list a n b c d e f)))
             (list (-> :g () 3)
                   (parallel-flow)
                   (-> :g (r)
                     (put r)))
             (flow:serially
               (-> :g () 1)
               (serial-flow)
               (-> :g (a)
                 (put a)))
             (-> :g ()
               (mt:open-latch latch))))))
    (5am:is (equal '(0 (2 nil 3 14 4 5 6) ((3 4 5)) 6) (nreverse result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flow-gen (p)
  (if p
      (>> (-> :g () 1))
      (-> :g () 2)))

(5am:test dynamic-flow
  (let ((result (list)))
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (-> :g ()
               (put 0)
               t)
             (->> (v) (flow-gen v))
             (-> :g (v)
               (put v)
               nil)
             (flow:dynamically (v) (flow-gen v))
             (-> :g (v)
               (put v))
             (-> :g ()
               (put 3)
               (mt:open-latch latch))))))
    (5am:is (equal '(0 1 2 3) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test concurrent-null-flow
  (let ((result (list)))
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (~> nil
                 (list nil))
             (-> :g ((a (b)))
               (put a)
               (put b)
               (mt:open-latch latch))))))
    (5am:is (equal '(nil nil) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test asynchronous-flow
  (let ((result (list)))
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (-> :g ()
               (put 1)
               2)
             (asynchronously (val)
               (put 2)
               (flet ((%continue ()
                        (continue-flow (+ 1 val))))
                 (run-it (-> :heh ()
                           (funcall #'%continue)))))
             (%> (val)
               (put val)
               (flet ((%continue ()
                        (continue-flow (+ 2 val))))
                 (run-it (-> :heh ()
                           (funcall #'%continue)))))
             (-> :g (val)
               (put val)
               (mt:open-latch latch))))))
    (5am:is (equal '(1 2 3 5) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test restarted-flow
  (let ((result (list)))
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (-> :g ()
               (error 'recoverable-condition))
             (-> :g (value)
               (put value)
               (error 'skipping-condition))
             (-> :g (value)
               (unless value
                 (put 0)))
             (-> :g ()
               (mt:open-latch latch))))))
    (5am:is (equal '(-1 0) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test non-concurrent-flow
  (let ((value 0))
    (flet ((increment ()
             (let ((v value))
               (sleep 0.05)
               (setf value (1+ v)))))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (~> (-> :one ()
                   (increment))
                 (-> :one ()
                   (increment))
                 (-> :one ()
                   (increment)))
             (-> :p ()
               (mt:open-latch latch))))))
    (5am:is (= 3 value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test fully-concurrent-flow
  (let ((value 0))
    (flet ((increment ()
             (let ((v value))
               (sleep 0.1)
               (setf value (1+ v)))))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (~> (-> :one ()
                   (increment))
                 (-> :two ()
                   (increment))
                 (-> :three ()
                   (increment)))
             (-> :p ()
               (mt:open-latch latch))))))
    (5am:is (= 1 value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
