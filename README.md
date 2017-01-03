# CL-FLOW
Experimental data-flow driven concurrency library for Common Lisp.


## Quick overview

`>>` operator means forms it encloses gonna be executed serially, but possibly in different
threads.

`~>` operator denotes forms that gonna be run in parallel.

`->` marks atomic block of code that can be dispatched concurrently.

Keywords in this examples denote invariants, so `->` block marked with the same invariant
shouldn't be executed concurrently. But this solely must be enforced by dispatcher that
implements `dispatch` generic function and passed into `run-flow` function.

Code is fully asynchronous, no blocking required (see Memory Model note below). Results of
previously executed block (denoted by `->`) "flow" into next code block and bound as arguments
to this next block.

At the end of the day, this approach is just glorified and syntactically sugared mix of promises
and data-flow model.


```lisp
(>> (-> :thread-0 ()
      (this "will be executed first")
      1)
    (-> :thread-1 (arg)
      (here "we will receive 1 from previous block as an argument" arg)))


(>> (~> (-> :thread-0 ()
          (this "will be executed in parallel")
          "pa")
        (-> :thread-1 ()
          (with "this code")
          "ral")
        (-> :thread-2 ()
          (and "this too")
          "lel"))
    (-> :any-thread (a b c)
      (here "we will receive (\"pa\") (\"ral\") (\"lel\") as" a b c)
      (in "exactly same order: order of results is preserved"
          "even if the functions executed in paralel")))
```

## Example

```lisp
(let ((out *standard-output*))
  (run-flow *dispatcher*
            (>> (~> (-> :tag-0 () "Hello")
                    (-> :tag-1 () ", concurrent"))
                (-> :tag-2 (a b)
                  (concatenate 'string (car a) (car b) " World!"))
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
