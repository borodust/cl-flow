# CL-FLOW

Library for asynchonous non-blocking concurrency in Common Lisp.


## Documentation
[Documentation](https://borodust.org/projects/cl-flow/) at [borodust.org](https://borodust.org)

You can hear my lengthy-bad-english explanation of `cl-flow` operation in
[Common Lisp Study Group: Concurrency in Common Lisp Part
2](https://youtu.be/nJ58pBCxdm8?t=548) video from `Atlanta Functional
Programming` group.

## Tests

```lisp
(ql:quickload :cl-flow/tests)
(5am:run! :cl-flow-suite)
```
