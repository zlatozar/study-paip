## Chapter 1

- With `@` in `examples.lisp`, page in the book is referred - with `:=>` expected result
e.g. `((+ 2 2) => 4 @ 4)`.
- Lisp is case **insensitive**.
- Lisp output is in UPPERCASE. In this way differ between input and output.
Slime also made UPPERCASEd output in red.
- Every expression return a value.

**atoms** - Atoms are the elementary LISP expressions. They are separated from other
expressions by blanks, parentheses or a line feed. Atoms are not character strings
contained in parentheses e.g. ```(a b c)``` is a list.
**symboilc atoms** (also called **s-atom**) - Atoms are distinguished between numbers and
those atoms like names e.g. `bread`, `milk`. _"Symbolic"_ because they can stand for
something else (e.g., as the name of a function or as a variable). In other words, you can
get a value assigned. Terms that s-atoms represent may begin with characters or digits
(but if it is a digit then the atom is a number) but may not contain a colon `:` or
semi-colon `;` nor parentheses or spaces. In LISP, parentheses and spaces (blanks) are
interpreted as separators of atoms and lists.
**numbers** - A special type of atom is the number. Unlike symbolic atoms, numbers have
the particular feature of being unable to accept values as they already have a constant
value, namely the number itself.
**expressions** - things that are evaluated to produce a value
(Lists and atoms belong to the generic term EXPRESSION)
**statements** - things that express an action

For instance, in Python, `x = 1` is a _statement_, and `(x + 1)` is an _expression_.
By making everything an expression, however, Lisps remove this limitation.
Since expressions are nestable, anything in the language can be combined with
nearly anything else.

- Consing with an example:
```cl
(defun replace-third (item list)
       (cons (first list)
             (cons (first (rest list))
                   (cons item (rest (rest (rest list)))))))
```
- Anything in LISP that is not explicitly NIL or the empty list or if the evaluation does
  not evaluate to NIL, has the truth value T!
- The last expression is returned as the value of the function call.
- Slime: Use `C-c C-j` to send expression to REPL.
- If you change code for experiment run ```(ql:quickload :study-paip)``` before.
- Special forms are expressions that return a value.
- The term special form is used confusingly to refer both to symbols like setf and
expressions that start with them, like `(setf x 3)`.
- What is the difference between `list` and `append` ?
- The symbol `nil` and the form () are completely synonymous; they are both
 representations of the empty list, `nil` is also used to denote the `false` value in Lisp.
- Function definition form:

``` cl
(defun function-name (parameter...)
  "documentation string"
  function-body...)
```
- The funny #' notation maps from the name of a function to the function itself.
It denotes function when this function is not in 'functional position' (first in a list)
- Use `C-c C-d C-d` to see documentation with Slime or in REPL `(?? '<symbol>)`
- Only the value `nil` is considered false; all other values are considered true.
- Use `trace` to see passed values to the function then `untrace`
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
In `else` we return correct answer so we do not need to touch it. Here is how to
deal with first part:

```
(first-name (name)
   (if (the first element of name is a title)
      (then return the first-name of the rest of the name) ;; recur
      (else return the first element of the name)))
```

Every recursive call chops off the first element and looks at the rest,
so for an n-element list there can be at most n recursive calls.

- `apply` is useful when the argument list is known only at runtime, especially when the arguments are read dynamically as a list.
You can still use `funcall` here but you have to unpack the individual arguments from the list, which is inconvenient.
You can also use `apply` like `funcall` by passing in the individual arguments.
The only thing it requires is that the last argument must be a list:

``` cl
(funcall #'+ 1 2)
(apply #'+ 1 2 ())
```
- `-` with one argument gives negative number e.g. `(- 1)`
- With lambda expressions it is possible to create new functions at run time.
And do not forget parenthesis around lambda e.g.
``` cl
(funcall #'(lambda (x) (+ x 2)) 4)
```
LAMBDA is a macro. It expands to `(function (lambda ...))`, which is the equivalent of
`#'(lambda ...)`. It allows to easily identify actual functions used as values. It also
makes it easier to replace with an flet or labels function. All this are the same:

```cl
(lambda (x) ...)
'(lambda (x) ...)
#'(lambda (x) ...) ; This one is used in book.
```
- What is the difference between reading and evaluating an expression?
- Every `atom` is either symbol or nonsymbol. A nonsymbol atom evaluates to itself.
- With LISP the ordering of elements in a list does matter.
- Like spaces, parentheses are separators, too. Therefore, the expression ```(abc(def))``` is a
  list containing the elements ```abc``` and ```(def)```. However, it is recommended to separate
  elements in a list by spaces to enhance the readability of expression, e.g., ```(abc (def))```.
- The empty list **NIL**
  A particular list called the empty list, contains no elements. They are simply written
  using matching parentheses: ```()```
  The empty list plays a particular role in LISP and therefore has been given a name: ```NIL```

  As a sole expression in LISP, the _empty list_ or _NIL_ has a particular status. It is a
  list as well as an atomical expression with the value of **NIL**. And, if seen as a logical
  expression, NIL can be interpreted as the truth value of FALSE.

- The symbol **T**
  The counterpart to the symbol NIL is the symbol **T** (true). This symbol also has itself as a
  value. **T** is a symbol and therefore an atom.
- Trees in Lisp are represented as nested lists. Try to solve exercise 1.4.
- If you get error in REPL to not hurry to jump in SLDB. First see the error
  explanation in REPL
- Think of `(mapcar #'last-name names)` as:
```
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names)
      ......)
```
- The compilation error message:
```
; caught ERROR:
;   illegal function call
```
almost always means that function is not defined or couldn't be found
- Take [interactive Lisp course|http://art2.ph-freiburg.de/Lisp-Kurs]
- Start solving [lisp koans|http://github.com/google/lisp-koans]

    * Load `contemplate.lsp` with Slime `C-c C-l`
    * Switch to `lisp-koans` package
    * Fill the blanks
    * Send s-expression to REPL to see result `C-c C-j`. If it is correct should return `T`
    * When finish with koan file reload `contemplate` (choose _[CONTINUE]_ Go ahead and
change the value.). In this way next koan file will be suggested.

Tip: _In programming the you can't see data only algorithms on that data. In Excel is vice
    versa._
