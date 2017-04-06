(in-package :cl-flow.tests)

(5am:def-suite :cl-flow-suite)

(5am:in-suite :cl-flow-suite)


(define-flow serial-flow
  (>> (loop repeat 5
         collecting (-> :p (a)
                      (1+ a)))))


(define-flow parallel-flow
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
           (-> :tag-2 (a b)
             (concatenate 'string (car a) (car b) " World!"))
           (-> :tag-3 (text)
             (setf result text)
             (mt:open-latch latch)))))
    (5am:is (equal "Hello, concurrent World!" result))))

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
             (-> :g (a n b c l)
               (destructuring-bind ((d) (e) (f g)) l
                 (put (list (car a) (car n) (car b) (car c) d e f g))))
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
    (5am:is (equal '(0 (2 nil 3 14 4 5 6 -1) ((3) (4) (5)) 6) (nreverse result)))))


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
             (-> :g (a b)
               (put (car a))
               (put (car b))
               (mt:open-latch latch))))))
    (5am:is (equal '(nil nil) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
