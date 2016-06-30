## Chapter 9

There are **four** very general and language-independent techniques for speeding
up an algorithm:

    * _Caching the results_ of computations for later reuse.

```memo`` works by returning a function that has an internal hash-table. When that function is
called, it first checks its hash-table to see if it has been called with the same
arguments before. If so, it returns the value it had calculated the first time it was
called. If it hasn’t been called with the same arguments before, the function will
instead call the function that was passed in to memo, and then store the result of that
inside the table. This way, if the memoized function is called with the same arguments a
second time, it can just look up the result in the table.

    * _Compiling_ so that less work is done at run time.
    * _Delaying the computation_ of partial results that may never be needed.
    * _Indexing_ a data structure for quicker retrieval.

#### SBCL profiling

Let's profile ```main``` from ```foo``` package.

```cl
CL-USER> (require :sb-sprof)
("SB-SPROF")
CL-USER> (sb-sprof:with-profiling (:report :flat)
           (foo:main 40))
```
