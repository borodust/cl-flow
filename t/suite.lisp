(in-package :cl-flow.tests)

(def-suite :cl-flow-suite)

(in-suite :cl-flow-suite)


(define-flow serial-flow
  (loop repeat 5
     collecting (-> :p (a)
                  (1+ a))))


(define-flow parallel-flow
  (~> (loop for i from 0 below 3
         collecting (let ((i i))
                      (-> :p (a)
                        (+ a i))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test simple-example
  (let ((result ""))
    (mt:wait-with-latch (latch)
      (run-it
       (>> (~> (-> :tag-0 () "Hello")
               (-> :tag-1 () ", concurrent"))
           (-> :tag-2 (a b)
             (concatenate 'string (car a) (car b) " World!"))
           (-> :tag-3 (text)
             (setf result text)
             (mt:open-latch latch)))))
    (is (equal "Hello, concurrent World!" result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(test complex-flow
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
             (-> :g (a b c l)
               (destructuring-bind ((d) (e) (f g)) l
                 (put (list (car a) (car b) (car c) d e f g))))
             (list (-> :g () 3)
                   (parallel-flow)
                   (-> :g (r)
                     (put r)))
             (>> (-> :g () 1)
                 (serial-flow)
                 (-> :g (a)
                   (put a)))
             (-> :g ()
               (mt:open-latch latch))))))
    (is (equal '(0 (2 3 14 4 5 6 -1) ((3) (4) (5)) 6) (nreverse result)))))
