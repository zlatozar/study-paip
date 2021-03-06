## Chapter 3

- Lisp Style Guide
  * Be specific
  * Use abstractions
  * Be concise
  * Use provided tools
  * Don't be obscure
  * Be consistent

- All the `def-` forms define global objects. It is also possible to define local variables
with `let`, and to define local functions with `label`.
- The `defstruct` special form defines a structure type and automatically defines functions
to get at components of the structure.
- Every time you try to reload a file that uses ```defconstant``` into the SLIME REPL,
it complains about me rebinding the constants.

Here is possible solutions:
    * Move constants into their own file, which, hopefully, doesn't change very often.
    * Change the constants into vars until you build a release candidate.
    In other words, temporarily change ```defconstant``` to ```defvar```.

- A _dotted list_ is like a list except that the CDR of the last cons does not have to be
  NIL. This name comes from the printed representation, which includes a 'dot' character
  (period). Here is an example: ```(a b . c)```

This dotted list is made of two conses. The CAR of the first cons is the symbol **a**, and the
CDR of the first cons is the second cons. The car of the second cons is the symbol **b**, and
the CDR of the second cons is the symbol **c**.

- The printed representation of a _structure_ starts with a `#S` and is followed by a list
consisting of the type of the structure and alternating pairs of slot names and values.
- Structures are more efficient than lists - less place and single step access.
- `when` and `unless` are preferred when there is only one possibility, `if` when there are
two, and `cond` when there are more than two.
- `setf` is a **generic setter**. `setf` understands place structures! From book p. 75
It contains the 'query' that finds the place where the new value should be set.
``` cl
(setf (get 'AL 'state) 'Alabama) ; (get 'AL 'state) => ALABAMA
```
Basically, the `setf` command do like this. Inner part is the **wish**: _"I would like to
do ```(get 'AL 'state)```?"_ - outer is the desired value so we have to read like this:
_"When I do ```(get 'AL 'state)``` I would like to receive ```ALABAMA```"_. In other
words - instead of a variable name, you pass it a complex Lisp expression that retrieves a
value. When you have a complicated, nested data structure, it's often easier to understand
code that retrieves data from a specific location. You put in one place the code that that
"gets at" this location and the value that you want to place. Using this feature, most
types of data structures cat get by without any specific "setting" functions of there own.

- In Lisp, the user can extend the expressions that are allowed in a `setf` form using the
special forms `defsetf` or `define-setf-method`.
- Some kinds of data, like binary trees, are hard to deal with in anything but a
recursive fashion. Others, like lists and integers, can be defined either recursively or as
a sequence. Read [here](binary-tree.lisp) how to represent trees in Common Lips.
- To be efficient recursive functions should be `tail-recursive`.
- As a good style write `RETURN` with uppercase to emphasise an unusual step to exit from
loop.
- `list*` is like `cons`
- In Lisp there are five major equality predicates, because not all objects are created
equally equal and also to increase generality. The numeric equality predicate, `=`, tests
if two numbers are the same.

Rest four begin with the letters `eq`, with more letters meaning the predicate considers
more objects to be equal. The simplest predicate is `eq`, which tests for the exact same
object. Next, `eql` tests for objects that are either `eq` or are equivalent numbers,
`equal` tests for objects that are either `eql` or are lists or strings with `eql`
elements. Finally, `equalp` is like `equal` except it also matches upper and lowercase
characters and numbers of different types.
- From Lisp Koans:
  * (eq x y) is true if and only if x and y are the same identical object
    eq is like comparing pointers in C. If the values are EQ, any non-nil
    value may be returned.
  * Additionally, Lisp also provides the type-specific predicates.
    For example, STRING= and STRING-EQUAL are predicates for strings.

- _#*0011_ defines a bit vector literal with four elements, 0, 0, 1 and 1
``` cl
(make-array '4 :element-type 'bit) ;=> #*0000
```
- How to create empty vector?
``` cl
(make-array 5 :fill-pointer 0 :adjustable t) ;=> #()
```
- The `association list` is a type of list used to implement tables.
- Hash tables are created with `make-hash-table`.
- The keys to hash tables are not restricted; they can be any Lisp object.
- A property list(plist) is list of altering key/value pairs.
With function GETF, which takes a plist and a symbol and returns the value in the plist
following the symbol, making a plist a sort of poor man’s hash table.
- An association list is also know as alist.
Alist give you more flexibility in how you do the lookup. Small alist could outperform
hash tables.
- Some functions are able to take effect beyond just computing the result - they have
`side effects`. In Lisp they are called `destructive`.
- Lisp types have `recognizer predicates`.
- Unfortunately, Common Lisp is not completely regular. There are no recognizers
for `fixnums`, `bignums`, `sequences`, and `structures`. Two recognizers, `null` and `atom`,
do not end in **p**. Also note that there is a hyphen before the **p** in `hash-table-p`,
because the type has a hyphen in it. In addition, all the recognizers generated by
`defstruct` have a hyphen before the **p**.
- The type hierarchy froms a graph, not just a tree.

```
Type         Example              Explanation
-----------------------------------------------------------------------------------
character    #\c                 A single letter, number, or punctuation mark.
number       42                  The most common numbers are floats and integers.
float        3.14159             A number with a decimal point.
integer      42                  A whole number, of either fixed or indefinite size:
fixnum       123                 An integer that fits in a single word of storage.
bignum       123456789           An integer of unbounded size.
function     #'sin               A function can be applied to an argument list.
symbol       sin                 Symbols can name fns and vars, and are themselves objects.
null         nil                 The object nil is the only object of type null.
keyword      :key                Keywords are a subtype of symbol.
sequence     (a b c)             Sequences include lists and vectors.
list         (a b c)             A list is either a cons or null.
vector       #(a b c)            A vector is a subtype of sequence.
cons         (a b c)             A cons is a non-nil list.
atom         t                   An atom is anything that is not a cons.
string       "abc"               A string is a type of vector of characters.
array        #lA(a b c)          Arrays include vectors and higher-dimensional arrays.
structure    #S(type ...)        Structures are defined by defstruct.
hash-table                       Hash tables are created by make-hash-table.
t            42                  Every object is of type t.
nil                              No object is of type nil.
complex      #C(0 1)             Imaginary numbers.
bit          0                   Zero or one.
rational     2/3                 Rationals include integers and ratios.
ratio        2/3                 Exact fractional numbers.
simple-array                     An array that is not displaced or adjustable.
readtable                        A mapping from characters to their meanings to read.
package      #lA(x y)            A collection of symbols that form a module.
pathname     #P"/usr/spool/mail  A file or directory name.
stream                           A pointer to an open file; used for reading or printing.
random-state                     A state used as a seed by random.
```

- Input in Lisp is incredibly easy because a complete lexical and syntactic parser is
available to the user. The parser is called `read`. It is used to read and return a single
Lisp expression.
- The expression passed by `read` need not be a legal _evaluable_ Lisp expression.
- `stream` is a Lisp name for a descriptor of Input/Output source e.g. terminal, file etc.
- The function `print` prints any object on a new line, with a space following it. `prin1`
will print any object without the new line and space. For both functions, the object is
printed in a form that could be processed by `read`. The function `princ` is used to print
in a human-readable format. This means that `read` cannot recover the original form; read
would interpret it as two symbols, not one string.
- Try `step` as a debugging tool. Remember that you can place it inside your function or
```cl
(step (foo <foo params>))
```
to start debugging from the very begging. Then in SLIME type usually 3 [STEP-INTO]
- Antibugging code checks for errors and possibly takes corrective action.
- `assert` could be very fancy.
*ASSERT* is ideal for those situations where your program's state must pass some test --
 an assertion.

``` cl
 (defun my-divide (numerator denominator)
   (assert (not (zerop denominator)))
   (/ numerator denominator))
```
What would be helpful is the ability to correct the offending value -- the zero denominator,
in this case -- and continue from the error. ASSERT's optional second argument lets you list places
whose values you might want to change to correct the problem.

``` cl
(defun my-divide (numerator denominator)
    (assert (not (zerop denominator)) (numerator denominator))
    (/ numerator denominator))
```
One last refinement to ASSERT lets you specify your own message to use when an assertion
fails.  By default, ASSERT may display the test form, but it is not required to do so. By
specifying a condition designator and arguments following the list of places, you can be
assured that you know what message will be printed upon an assertion failure.

``` cl
(defun my-divide (numerator denominator)
    (assert (not (zerop denominator)) (numerator denominator)
            "You can't divide ~D by ~D." numerator denominator)
    (/ numerator denominator))
```

- Understand `flet` and `labels`.
- With `closures` we create functions with state.
- Common Lisp provides for two kinds of variables: _lexical_ and _special_ variables.
- _special_ variables could be shadowed by a local binding for that variable.
- Functions could return many values. To access them use `multiple-value-bind`.
- Keywords evaluates to themselves.
- Keywords are constants, and so cannot be used as names of variables.
- Any time you put a `&key` in a parameter list, you need an `&allow-other-keys` if, in
fact, other keywords are allowed.
- Function alias:
``` cl
(setf (symbol-function 'find-all-if) #`remove-if-not)
```
- With `some` function you just implement **Chain of Responsibility** design pattern.
