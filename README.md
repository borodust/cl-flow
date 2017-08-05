# CL-FLOW
Experimental data-flow driven concurrency library for Common Lisp.


## Quick overview

`->` (`flow:atomically`) operator marks atomic block of code that could be dispatched
concurrently.

`>>` (`flow:serially`) operator encloses forms for them to be executed serially, but possibly in
different threads, returning the value of the last atomic block or flow.

`~>` (`flow:concurrently`) operator denotes forms that are going to be run in parallel, returning
list of form results in the same order they were specified.

`->>` (`flow:dynamically`) denotes block that generates new flow dynamically during parent flow
execution. In other words, injects new dynamically created flow into current one.


Keywords in this examples denote invariants, so `->` block marked with the same invariant
shouldn't be executed concurrently. But this must be enforced by dispatching function passed
into `run-flow`.

Code is fully asynchronous, no blocking required (see Memory Model note below). Results of
previously executed block (denoted by `->`) "flows" into next code block and is bound as
argument to this next block.

At the end of the day, this approach is just glorified and syntactically sugared mix of promises
and data-flow model.


## Documentation
[Wiki](https://github.com/borodust/cl-flow/wiki)


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

## NOTES
- Experimental
- Not extensively tested
- There's no clearly defined memory model for CL, so there might be issues with cached thread-local non-volatile variables/memory.
