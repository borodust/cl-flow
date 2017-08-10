# CL-FLOW
Experimental data-flow driven concurrency library for Common Lisp.


## Documentation
[Documentation](https://borodust.org/projects/cl-flow/) at [borodust.org](https://borodust.org)


## Example

```lisp
(let ((out *standard-output*))
  (run-flow *dispatcher*
            (>> (~> (-> :tag-0 () "Hello")
                    (-> :tag-1 () ", concurrent"))
                (-> :tag-2 ((a b))
                  (concatenate 'string a b " World!"))
                (-> :tag-3 (text)
                  (print text out)))))
```

## Tests

```lisp
(ql:quickload :cl-flow/tests)
(5am:run! :cl-flow-suite)
```
