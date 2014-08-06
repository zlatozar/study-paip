####Chapter 1####

- With @ in ```examples.lisp```, page in the book is referred - with => expected result
e.g. ((+ 2 2) => 4 @ 4).
- Lisp is case insensitive.
- Lisp output is in UPPERCASE. In this way differ between input and output.
Slime also made UPPERCASE red.
- Every expression return a value.
- The last expression is returned as the value of the function call.
- Slime: Use ```C-c C-j``` to send expression to REPL.
- If you change code for experiment run ```(ql:quickload :study-paip)``` before.
- Special forms are expressions that return a value.
- The term special form is used confusingly to refer both to symbols like setf and
expressions that start with them, like ```(setf x 3)```.
- What is the difference between ```list``` and ```append``` ?
- The symbol ```nil``` and the form () are completely synonymous; they are both representations of the empty list, ```nil```
is also used to denote the ```false``` value in Lisp.
- Function definition form:

``` cl
(defun function-name (parameter...)
  "documentation string"
  function-body...)
```
- The funny #' notation maps from the name of a function to the function itself.
- Use ```C-c C-d C-d``` to see documentation with Slime or in REPL ```(?? '<symbol>)```
- Only the value ```nil``` is considered false; all other values are considered true.
- Use ```trace``` to see passed values to the function then ```untrace```
- Here is how to deal with recursive functions:

Any function is required to return the correct value for the given input(s). Another
way to look at this requirement is to break it into two parts: a function must return
a value, and it must not return any incorrect values.

```
(first-name (name)
   (if (the first element of name is a title)
      (then do something complicated to get the first-name) ;; reduction step
      (else return the first element of the name)))
```
In ```else``` we return correct answer so we do not need to touch it. Here is how to
deal with first part:

```
(first-name (name)
   (if (the first element of name is a title)
      (then return the first-name of the rest of the name) ;; recur
      (else return the first element of the name)))
```

Every recursive call chops off the first element and looks at the rest,
so for an n-element list there can be at most n recursive calls.

- ```apply``` is useful when the argument list is known only at runtime, especially when the arguments are read dynamically as a list.
You can still use ```funcall``` here but you have to unpack the individual arguments from the list, which is inconvenient.
You can also use ```apply``` like ```funcall``` by passing in the individual arguments.
The only thing it requires is that the last argument must be a list:

``` cl
(funcall #'+ 1 2)
(apply #'+ 1 2 ())
```
- ```-``` with one argument gives negative number e.g. (- 1)
- With lambda expressions it is possible to create new functions at run time.
And do not forget parenthesis around lambda e.g.
``` cl
(funcall #'(lambda (x) (+ x 2)) 4)
```
- What is the difference between reading and evaluating an expression?
- Every ```atom``` is either symbol or nonsymbol. A nonsymbol atom evaluates to itself.
- Trees in Lisp are represented as nested lists. Try to solve exercise 1.4.
- Think of ```(mapcar #'last-name names)``` as:
```
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names)
      ......)
```
- Start solving lisp koans: http://github.com/google/lisp-koans

    * Load ```contemplate.lsp``` with Slime ```C-c C-l```
    * Switch to ```lisp-koans``` package
    * Fill the blanks
    * Send s-expression to REPL to see result ```C-c C-j```. If it is correct should return ```T```