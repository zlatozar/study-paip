## Lisp Style

Read also **25.15 A Style guide to Lisp** on p. 887

#### Widely used "need-to-know" conventions

| Example           | Description                                             |
| :---------------- | :--------------------------------                       |
| `foo-bar`         | "-" is used as a word delimiter                         |
| `*foo*`           | (global) special variable                               |
| `foo*`            | slightly different variant of the `foo` operator        |
| `nfoo`            | (possibly) destructive (non-consing) function           |
| `foop`            | predicate (also `foo-p`, be consistent)                 |
| `foof`            | place changing (like in **SETF**, **INCF**, ...)        |
| `+foo+`           | constant, or single `CLOS` instance                     |
| `%foo`            | low-level, fast, dangerous function, or system specific |
| `make-foo`        | create a `fo`o and return it                            |
| `define-foo`      | (globally) define a new `foo`                           |
| `with-foo`        | create a dynamic context with a `foo`                   |
| `do-foo`          | iterate through a `foo`                                 |
| `foo-case`        | foo-specific case expression                            |
| `foo-bar`         | type-slot, converting **FOO** to **BAR**                |
| `foo-to-bar`      | converting **FOO** to **BAR**                           |
| `<class-name>`    | Surround class name with "<" and ">"                    |

#### Comments

**80 column** maximum width!

`;`     for inline comment<br/>
`;;`    for in function comment<br/>
`;;;`   for between function comment<br/>
`;;;;`  for section header

#### Multi-Line Strings

In case of a multi-line string as a literal constant consider
instead using read-time evaluation and a call to format:
``` cl
(defclass document () ()
  (:documentation #.(format nil "Hey look, this is a ~
                                 very nice multi-line string ~
                                 and a haiku too.")))
```

#### Naming Functions And Variables

- Functions are usually actions. Therefore name them accordingly with a verb first then an
  object:
```
    generate-first-deck
    pick-letter
    begin-game
```
Some functions return a boolean. In this case put the subject first and the verb next:
```
    word-exist
    number-is-even
    file-lock
```
This is so that conditional statements resemble English:
```
    (if (word-exist
    (when (not file-lock
```
**Avoid double negative like the pest!**

- Name variables as explicitly as possible, mostly use two word when one is not sufficient.
- Never put and **'s'** at the end, it's a source of typos.
- Don't abbreviate character to **char** or index to **i**. What you gain in typing is lost
in refactoring or code reading. Here are some variables examples:
```
    time-left
    bad-word
    deck-letter
```

#### Class

Add `:type` to each slots

``` cl
(defclass <aluminium> (<metal>)
    ((color :type string
            :initarg :color
            :initform "white")
     (solidity :type (or integer <solidity>)
               :initarg :solidity
               :initform (make-instance '<solidity>))
     (cost :type (or integer null)
           :initarg :cost))
  (:documentation "A class represents Aluminium."))
```

Don't forget a type `null` for optional slots.

### General Programming Style rules

- Write short functions, where each function provides a single, well-defined
operation. Small functions are easier to read, write, test, debug, and understand.
- Use descriptive variable and function names. If it isn't clear from the name of a
function or variable what its purpose is, document it with a documentation string and a
comment.
- Use proper indentation -- you should be able to understand the structure of your
definitions without noticing the parentheses.
- The general rule of thumb about `EVAL` is: if you think you need to use EVAL, you're
probably wrong.
- `CATCH` and `THROW`. Often a named `BLOCK` and `RETURN-FROM` are more appropriate. Use
`UNWIND-PROTECT` when necessary.
- Destructive operations, such as `NCONC`, `SORT`, `DELETE`should be used carefully
and sparingly.

### Readability of Code

- Don't use any C{A,D}R functions with more than two letters between the C and the R. When
nested, they become hard to read. If you have complex data structures, you are often
better off describing them with a `DEFSTRUCT`, even if the type is LIST. The data
abstraction afforded by `DEFSTRUCT` makes the code much more readable and its purpose
clearer. If you must use C{A,D}R, try to use `DESTRUCTURING-BIND` instead, or at least
`SECOND`, `THIRD`, `NTH`, `NTHCDR`, etc.
- If the second argument to `IF` is the same as the first, use `OR` instead: `(or P Q)`
rather than `(if P P Q)`.
- Use `UNLESS` instead of `(when (not ..) ..)` but not instead of `(when (null ..) ..)`.
- Use `COND` instead of nested IF statements. Be sure to check for **unreachable cases**,
and eliminate those cond-clauses.
- Use backquote, rather than explicit calls to `LIST`, `CONS`, and `APPEND`, whenever
writing a form which produces a Lisp form, but not as a general substitute for `LIST`,
`CONS` and `APPEND`.
- Some people prefer to use macros to define constants, since this avoids the problem of
accidentally trying to bind a symbol declared with `defconstant`.
- If you intend for a function to be a predicate, have it return **T** for true, not just
non-NIL. If there is nothing worth returning from a function, returning **T** is
conventional. But if a function is intended to be more than just a predicate, it is better
to return a useful value. (For example, this is one of the differences between `MEMBER`
and `FIND`.)
- When `NIL` is used as an empty list, use `()` in your code. When `NIL` is used as a
boolean, use `NIL`. Similarly, use `NULL` to test for an empty list, `NOT` to test a
logical value. Use `ENDP` to test for the end of a list, not `NULL`.
- Don't use the `&AUX` lambda-list keyword. It is always clearer to define local variables
using `LET` or `LET*`.
- If you want a function to return no values (i.e., equivalent to `void` in C), use
`(VALUES)` to return zero values. This signals to the reader that the function is used
mainly for side-effects.

### Macros

- When defining a macro that provides an implicit `PROGN`, use the `&BODY` lambda-list
keyword instead of `&REST`.
- Use gensyms for bindings within a macro, unless the macro lets the user explicitly
specify the variable.
- Use a **DO-** prefix in the name of a macro that does some kind of iteration, **WITH-**
when the macro establishes bindings, and **DEFINE-** or **DEF-** when the macro creates
some definitions. Don't use the prefix **MAP-** in macro names, only in function names.
- Don't define a macro where a function definition will work just as well -- remember, you
can `FUNCALL` or `MAPCAR` a function but not a macro.

### Stylistic Preferences

- Use `INCF`, `DECF`, `PUSH` and `POP` instead instead of the corresponding `SETF` forms.
- Don't use `LET*` where `LET` will do. Don't use `LABELS` where `FLET` will do. Don't use
`DO*` where `DO` will do.
- Don't use `DO` where `DOTIMES` or `DOLIST` will do.
- If you like using `MAPCAR` instead of `DO/DOLIST`, use `MAPC` when no result is needed
-- it's more efficient, since it doesn't cons up a list.
- If a single cumulative value is required, use `REDUCE`.
- If you are seeking a particular element, use `FIND`, `POSITION` or `MEMBER`.
- If using `REMOVE` and `DELETE` to filter a sequence, don't use the `:test-not` keyword
or the `REMOVE-IF-NOT` or `DELETE-IF-NOT` functions.
- Use `COMPLEMENT` to complement the predicate and the `REMOVE-IF`or `DELETE-IF` functions
instead.
- Don't use lists where vectors are more appropriate. Accessing the nth element of a
vector is faster than finding the nth element of a list, since the latter requires pointer
chasing while the former requires simple addition. Vectors also take up less space than
lists.
- Use adjustable vectors with fill-pointers to implement a stack, instead of a list --
using a list continually conses and then throws away the conses.
- When adding an entry to an association list, use `ACONS`, not two calls to `CONS`. This
makes it clear that you're using an **alist**.
- If your association list has more than about 10 entries in it, consider using a hash
table. Hash tables are often more efficient.
- When you don't need the full power of **CLOS**, consider using structures instead. They
are often faster, take up less space, and easier to use.
- Use `WITH-OPEN-FILE` instead of `OPEN` and `CLOSE`.
- When a `HANDLER-CASE` clause is executed, the stack has already unwound, so dynamic
bindings that existed when the error occured may no longer exist when the handler is
run. Use `HANDLER-BIND` if you need this.
- When using `CASE` and `TYPECASE` forms, if you intend for the form to return `NIL` when
all cases fail, include an explicit `OTHERWISE` clause. If it would be an error to return
`NIL` when all cases fail, use `ECASE`, `CCASE`, `ETYPECASE` or `CTYPECASE` instead.
