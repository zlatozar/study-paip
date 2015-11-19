## Chapter 2

Here is how we can represent simple tiny English grammar:

```
Sentence    => Noun-Phrase + Verb-Phrase
Noun-Phrase => Article + Noun
Verb-Phrase => Verb + Noun-Phrase
Article     => the, a,...
Noun        => man, ball, woman, table...
Verb        => hit, took, saw, liked...
```
This description is called a _context-free phrase-structure grammar_, and
the underlying paradigm is called _generative syntax_. It called generative because:

To get a _Sentence_, append a _Noun-Phrase_ and a _Verb-Phrase_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;To get a _Noun-Phrase_, append an _Article and a _Noun_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Choose _"the"_ for the _Article_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Choose _"man"_ for the _Noun_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;The resulting _Noun-Phrase_ is _"the man"_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;To get a _Verb-Phrase_, append a _Verb_ and a _Noun-Phrase_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Choose _"hit"_ for the _Verb_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;To get a _Noun-Phrase_, append an _Article_ and a _Noun_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Choose _"the"_ for the _Article_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Choose _"ball"_ for the _Noun_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The resulting _Noun-Phrase_ is _"the ball"_<br/>
&nbsp;&nbsp;&nbsp;&nbsp;The resulting _Verb-Phrase_ is _"hit the ball"_<br/>
The resulting _Sentence_ is _"The man hit the ball"_<br/>

A _Rule-Based Solution_ = concentrate on making it easy to write grammar rules and would
worry later about how they will be processed.

**"Use the most natural notation available to solve a problem."**

- If you want to trace whole program path pass all function and it's helper one to `trace`.
``` cl
;; Note that you don't quote and you can add many
(trace sentence noun-phrase verb-phrase article noun verb)
```
- When implement grammar rule we strive to use only linguistic conventions.
- What is the difference between `defparameter` and `defvar`?

Use `defvar` for things you **don't want** to _re-initialize up on re-load_.
``` cl
(defvar *options*)
(defun add-option (x) (pushnew x *options*))
```
Here you might have done *(add-option ...)* many times before you re-load files - perhaps
some even from another file. You usually don't want to throw all that data just because you re-load
this definition.

On the other hand some kinds of options **do want** to get _re-initialized up on re-reload_
then use `defparameter`.

- A change to a parameter is considered a change to the program, not a change by the
program.
- **The craft of programming includes knowing what not to write, as well as what to write.**
- `data-driven` programming is, when data drives what the program does next.
- Work with the problem as much as possible in its own terms, and to minimize the part of
the solution that is written directly in Lisp.
- The advantage of representing information in a _declarative form_ - as rules or facts
rather than as Lisp functions - is that it can be easier to use the information for multiple
purposes.

- Try to understand `combine-all` function on p.45.

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
in _'mappend'_. Note that in our first step we hard coded **'(1)**, **'(2)** and so on.
_We introduce function state!_

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

Try to solve exercise **2.4** now _(or carefully read the answer)_.

See _Chapter 9_ for optimization. `generate` is now interpretor of the grammar but it is
possible to _compile_ rules.
