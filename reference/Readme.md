##Language Reference##

###Syntax###

Entries for functions are lists beginning with the function name, followed by an
indication of the parameter list. Entries for special operators and macros are regular
expressions indicating the form of a valid call.

In a regular expression, something followed by an asterisk indicates zero or more of them:
(a\*) could be (), or (a), or (a a), and so on. Something in square brackets indicates
zero or one of them: (a [b] c) could be (a c) or (a b c). Curly brackets are sometimes
used for grouping: ({a b}\*) could be () or (a b), or (a b a b), and so on. A vertical bar
indicates a choice between several alternatives:
(a {1 | 2} b) could be (a 1 b) or (a 2 b).

###Parameter Names###

The parameter names correspond to restrictions on the arguments. If a parameter has the
name of a type, then the corresponding argument must be of that type. Additional
implications of parameter names are listed in the following table.

Arguments to macros and special operators are not evaluated unless the description says so
explicitly. If such an argument is not evaluated, type restrictions implied by its name
apply to the argument itself; if it is evaluated, they apply to its value. If a macro
argument is evaluated, it is evaluated in the environment where the macro call appears,
unless the description explicitly says otherwise.

```alist```
must be an assoc-list, which is a proper list whose elements, if any, are of the form
_(key . value)_.

```body```
indicates the arguments that could come after the parameter list in a **defun** expression: either
_declaration\* [string] expression\*_,
or
_[string] declaration\* expression\*_.

So an entry like `(def un fname parameters . body)` indicates that the syntax of a
**defun** expression could be

(**defun** _fname parameters declaration\* [string] expression\*_),
or
_(**defun** fname parameters [string] declaration\* expression\*)_.

If the string is followed by at least one expression, it is interpreted as a documentation string.

```c```
must be a complex number.

```declaration```
must be a list whose car is **declare**.

```environment```
indicates an object representing a lexical environment. (You can't create such objects
directly, but Lisp uses them internally.) The symbol **nil** always represents the global
environment.

```f```
must be float.

```fname```
must be a function name: either a symbol or a list **(setf s)**, where **s** is a symbol.

```format```
can be either a string that could be the second argument to **format**, or a function that
takes a stream and some optional arguments and (presumably) writes something to the
stream.

```i```
must be integer.

```list```
can be a list of any type. Whether or not a list can be circular depends on the
context. Functions that take lists as arguments can take **cdr**-circular lists if and only
if their purpose never requires them to find the end of the list. So **nth** can take
**cdr**-circular lists, but **find** cannot. A parameter called `list` can always be
**car**-circular.

```n```
must be a non-negative integer.

```object```
can be of any type.

```package```
must be a package, a string that is the name of a package, or a symbol whose name is the
name of a package.

```path```
can be a pathname, a stream associated with a file (in which case it indicates the
pathname used to open the stream), or a string.

```place```
must be an expression that could be the first argument to **setf**.

```plist```
must be a property list, which is a proper list with an even number of elements.
E.g. (:a 1 :b 2)

```pprint-dispatch```
must be a pprint dispatch table (or possibly **nil**).

```predicate```
must be a predicate function.

```prolist```
must be a proper list.

```proseq```
must be a proper sequence—that is, a vector or a proper list.

```r```
must be a real.

```tree```
mposes no type restriction—everything is a tree. But cannot be a **car-** or **cdr**-circular
list.

```type```
must be a type designator.

###Defaults###

Certain optional parameters always have the same default. An optional _stream_ parameter
always defaults to \*standard-input\* or \*standard-output\*, depending on whether the
operator in question is for use on input or output streams. An optional parameter called
_package_ always defaults to \*package\*; one called _readtable_ always defaults to
\*readtable\*; and one called **pprint-dispatch** always defaults to
\*print-pprint-dispatch\*.

###Comparison###

Many functions that compare sequence elements take the keyword arguments **key, test,
test-not, from-end, start,** or **end**. Their use is the same in every case. The **key,
test** and **test-not* arguments must be functions, and the **start** and **end**
arguments must be non-negative integers. In the descriptions of functions that take such
keyword arguments, words like "match", "member", and "element" are to be understood as
modified by the presence of such arguments.  In any function that does comparisons on
sequence elements, it is also to be assumed that the default test for equality is **eql**,
unless stated otherwise.

###Structure###

If an operator returns structure (e.g. lists), it should be understood that the return
value can share structure with objects passed as arguments, unless the description says
that the return value is newly created. However, only parameters shown in **[]** brackets
_([list])_ can actually be modified by the call. If two functions are listed together, the
second is a destructive version of the first.


###Evaluation and Compilation###

`(compile fname &optional function)` -> Function

If function is not provided and _fname_ is the name of an uncompiled function or macro,
then replaces that function or macro with a compiled version, returning _fname_. (The
lexical environment in which the function or macro is defined should not differ from the
global environment except by having local macro or symbol-macro definitions, or
declarations.) If _function_ (which may be a function or lambda expression) is provided,
then coerces it to a function, compiles it, and names it _fname_, returning _fname_. The
_fname_ may also be **nil** , in which case the compiled function is returned. Returns two
additional values: a second true iff compilation generated errors or warnings; and a third
true iff compilation generated errors or warnings other than style warnings.



###Types and Classes###


###Control and Data Flow###


###Iteration###


###Objects###


###Structures###


###Conditions###


###Symbols###


###Packages###


###Numbers###


###Characters###


###Conses###


###Arrays###


###Strings###


###Sequences###


###Hash Tables###


###Filenames###


###Files###


###Streams###


###Printer###


###Reader###


###System Construction###


###Environment###


###Constants and Variables###


###Type Specifiers###


###Read Macros###
