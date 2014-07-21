####Chapter 2####

- If you want to trace whole program path pass all function and it's helper one to ```trace```.
``` cl
(trace sentence noun-phrase verb-phrase article noun verb)
```
- When implement grammar rule we strive to use only linguistic conventions.
- What is the difference between ```defparameter``` and ```defvar```?

Use `defvar` for things you don't want to re-initialize up on re-load.
``` cl
(defvar *options*)
(defun add-option (x) (pushnew x *options*))
```
Here you might have done *(add-option ...)* many times before you re-load files - perhaps
some even from another file. You usually don't want to throw all that data just because you re-load
this definition.

On the other hand some kinds of options do want to get re-initialized up on re-reload then use
`defparameter`.
- ```assoc``` takes two arguments, a "key" and a list of lists, and returns the first
element of the list of lists that starts with the key. If there is none, it returns ```nil```.
- The craft of programming includes knowing what not to write, as well as what to write.
- ```data-driven``` programming is, when data drives what the program does next.
- Work with the problem as much as possible in its own terms, and to minimize the part of
the solution that is written directly in Lisp.
- The advantage of representing information in a declarative form—as rules or facts
rather than as Lisp functions—is that it can be easier to use the information for multiple
purposes.
- Try to understand ```combine-all``` function.

Here is how function should work:
``` cl
(combine-all '((a) (b)) '((1) (2))) => ((A 1) (B 1) (A 2) (B 2))
```

_'mapcar'_ with _'append'_ function almost do the job:
``` cl
(mapcar #'(lambda (x y) (append x y)) '((a) (b)) '((1) (2))) => ((A 1) (B 2))
```

Hm, it seams that we need finish with **'a'** then to repeat the procedure with **'b'**.
Let's hard code argument. Something like that:
``` cl
(mapcar #'(lambda (x) (append x '(1))) '((a) (b))) => ((A 1) (B 1))
(mapcar #'(lambda (x) (append x '(2))) '((a) (b))) => ((A 2) (B 2))
```

Results are correct and next step is a loop to unite results. _'mappend'_ comes naturally.

**Attention**: Combine steps are the tricky part!
How to create a function that must be passed to _'mappend'_?

We begin with one function then when first argument list exceeds we have to change argument
in _'mappend'_. Note that in our first step we hard coded **'(1)**, **'(2)** and so on. We introduce function state!

That clearly indicates **closure**! We wrap function with function
that dictates inner function state - basically preset one of it's parameters:

Here is the _pseudo_ code for the first iteration:
``` cl
(mappend #'(lambda ('(1)) (mapcar #'(lambda (x) (append x '(1))) '((a)) )) '((a) (b)))
```
Replace **'(1)** with **'y'** and voila:
``` cl
(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2))) -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))
```

Try to solve exercise 2.4 now (or carefully read the answer).
