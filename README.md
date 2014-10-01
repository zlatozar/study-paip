## Paradigms of AI Programming Source Code

This project contains the Lisp source code for the book **"Paradigms of
Artificial Intelligence Programming"** by [Peter Norvig](http://norvig.com). The
code is offered as open source freeware under this [License](#License).  In
addition, code was divided into packages and additional comments was added.

#### Installation Instructions

- `git clone` the source code from the github repository.

- You must have a lisp compiler/interpreter.

- `quicklisp` should be configured

- To test all the code, start lisp in the project directory and do the
  following at the interactive prompt:

``` cl
(ql:quickload :study-paip)
(tutor:do-examples :all)
```

This should print out a long list of inputs and outputs, and the last
output should be the total number of errors. If all goes well, this should be *"0"*.

If you would like to run the examples for just a given chapter, you can do that
as well. For instance, once you have done `(ql:quickload :study-paip)`, this will
execute the examples for _Chapter 3_:

``` cl
(tutor:do-examples 3)
```

or

``` cl
(tutor:do-chapter 3 *standard-output*)
```

In some chapters there are many versions of the program. Examples to this attempts are
marked with a second digit. For example for _Chapter 4_ if you want to run first
naive approach:

``` cl
(tutor:do-examples 4.1)
```

**Final version** is only with one digit:

``` cl
(tutor:do-examples 4)
````

NOTE: If you want to see code without packages names (not fully qualified) you
have to go to the chapter first. For example _Chapter 4_:

``` cl
(in-package :ch4-final)
(tutor:do-examples 4)
```

#### Typical Study Session

_(to be added)_

#### Git Tags (work in progress)

Every chapter is a git tag(s):

```$ git checkout ch1```

will give you only files needed for chapter one.

### What Lessons are in PAIP?
_by Peter Norvig_

```
1. Use anonymous functions. [p. 20]
2. Create new functions (closures) at run time. [p. 22]
3. Use the most natural notation available to solve a problem. [p. 42]
4. Use the same data for several programs. [p. 43]
5. Be specific. Use abstractions. Be concise. Use the provided tools. Don't be
   obscure.  Be consistent. [p. 49]
6. Use macros (if really necessary). [p. 66]
7. There are 20 or 30 major data types; familiarize yourself with them. [p. 81]
8. Whenever you develop a complex data structure, develop a corresponding
   consistency checker. [p. 90]
9. To solve a problem, describe it, specify it in algorithmic terms, implement
   it, test it, debug and analyze it. Expect this to be an iterative
   process. [p. 110]
10. AI programming is largely exploratory programming; the aim is often to
    discover more about the problem area. [p. 119]
11. A general problem solver should be able to solve different
    problems. [p. 132]
12. We must resist the temptation to belive that all thinking follows the
    computational model. [p. 147]
13. The main object of this book is to cause the reader to say to him or herself
    "I could have written that". [p. 152]
14. If we left out the prompt, we could write a complete Lisp interpreter using
    just four symbols.  Consider what we would have to do to write a Lisp (or
    Java) interpreter in Java. [p. 176]
15. Design patterns can be used informally, or can be abstracted into a formal
    function, macro, or data type (often involving higher-order
    functions). [p. 177]
16. Use data-driven programming, where pattern/action pairs are stored in a
    table. [p. 182]
17. Sometimes "more is less": its easier to produce more output than just the
    right output. [p. 231]
18. Lisp is not inherently less efficient than other high-level languages -
    Richard Fateman. [p. 265]
19. First develop a working program. Second, instrument it. Third, replace the
    slow parts. [p. 265]
20. The expert Lisp programmer eventually develops a good "efficiency
    model". [p. 268]
21. There are four general techniques for speeding up an algorithm: caching,
    compiling, delaying computation, and indexing. [p. 269]
22. We can write a compiler as a set of macros. [p. 277]
23. Compilation and memoization can yield 100-fold speed-ups. [p. 307]
24. Low-level efficiency concerns can yield 40-fold speed-ups. [p. 315]
25. For efficiency, use declarations, avoid generic functions, avoid complex
    argument lists, avoid unnecessary consing, use the right data
    structure. [p. 316]
26. "A language that doesn't affect the way you think about programming is not
    worth knowing"
    - Alan Perlis. [p. 348]
27. Prolog relies on three important ideas: a uniform data base, logic
    variables, and automatic backtracking. [p. 349]
28. Prolog is similar to Lisp on the main points. [p. 381]
29. Object orientation = Objects + Classes + Inheritance - Peter Wegner [p. 435]
30. Instead of prohibiting global state (as functional programming does),
    object-oriented programming breaks up the unruly mass of global state and
    encapsulates it into small, manageable pieces, or objects. [p. 435]
31. Depending on your definition, CLOS is or is not object-oriented.  It doesn't
    support encapsulation. [p. 454]
32. Prolog may not provide exactly the logic you want [p. 465], nor the
    efficiency you want [p. 472].  Other representation schemes are possible.
33. Rule-based translation is a powerful idea, however sometimes you need more
    efficiency, and need to give up the simplicity of a rule-based system
    [p. 509].
34. Translating inputs to a canonical form is often a good strategy [p. 510].
35. An "Expert System" goes beyond a simple logic programming system: it
    provides reasoning with uncertainty, explanations, and flexible flow of
    control [p. 531].
36. Certainty factors provide a simple way of dealing with uncertainty, but
    there is general agreement that probabilities provide a more solid
    foundation [p. 534].
37. The strategy you use to search for a sequence of good moves can be important
    [p. 615].
38. You can compare two different strategies for a task by running repeated
    trials of the two [p. 626].
39. It pays to precycle [p. 633].
40. Memoization can turn an inefficient program into an efficient one [p. 662].
41. It is often easier to deal with preferences among competing interpretations
    of inputs, rather than trying to strictly rule one interpretation in or out
    [p 670].
42. Logic programs have a simple way to express grammars [p. 685].
43. Handling quantifiers in natural languiage can be tricky [p. 696].
44. Handling long-distance dependencies in natural language can be tricky
    [p. 702].
45. Understanding how a Scheme interpreter works can give you a better
    appreciation of how Lisp works, and thus make you a better programmer
    [p. 753].
46. The truly amazing, wonderful thing about call/cc is the ability to return to
    a continuation point more than once. [p. 771]
47. The first Lisp interpreter was a result of a programmer ignoring his boss's
    advice. [p. 777].
48. Abelson and Sussman (1985) is probably the best introduction to computer
    science ever written [p. 777].
49. The simplest compiler need not be much more complex than an interpreter
    [p. 784].
50. An extraordinary feature of ANSI Common Lisp is the facility for handling
    errors [p. 837].
51. If you can understand how to write and when to use once-only, then you truly
    understand macros [p. 853].
52. A word to the wise: don't get carried away with macros [p. 855].
```

#### Errata

Page *285*: Replace definition of `filter` function with:

``` cl
(defun filter (pred pipe)
  "keep only items in (non-null) pipe satisfying predicate"
  (if (eq pipe empty-pipe)
    empty-pipe
    (if (funcall pred (head pipe))
      (make-pipe (head pipe) (filter pred (tail pipe)))
      (filter pred (tail pipe)))))
```


## License

Software License Agreement

Copyright © 1998-2002 by Peter Norvig.

Permission is granted to anyone to use this software, in source or object code
form, on any computer system, and to modify, compile, decompile, run, and
redistribute it to anyone else, subject to the following restrictions:

- The author makes no warranty of any kind, either expressed or implied,
  about the suitability of this software for any purpose.

- The author accepts no liability of any kind for damages or other
  consequences of the use of this software, even if they arise from defects in
  the software.

- The origin of this software must not be misrepresented, either by explicit
  claim or by omission.

- Altered versions must be plainly marked as such, and must not be
  misrepresented as being the original software. Altered versions may be
  distributed in packages under other licenses (such as the GNU license).

If you find this software useful, it would be nice if you let me
(peter@norvig.com) know about it, and nicer still if you send me modifications
that you are willing to share. However, you are not required to do so.
