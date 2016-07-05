- [Packages](#packages)
- [Formatting](#formatting)
- [Macros](#macros)

### Why lisp?

_Lisp is not merely a different notation, it's a fundamentally different way of thinking_
_about what programming is._ The mainstream model is that programming consists of
producing standalone artifacts called **programs** which operate on other artifacts called
**data**. Of course, everyone knows that programs are data, but the mainstream model revolves
around maintaining an artificial distinction between the two concepts. Yes, programs are
data, but they are data only for a special kind of program called a compiler. Compilers
are hard to write, a field of study unto themselves. Most people don't write their own
compilers, but instead use compilers written by the select few who have attainted the
level of mastery required to write one that isn't just a toy.

_The Lisp model is that programming is a more general kind of interaction with a machine._
The act of describing what you want the machine to do is interleaved with the machine
actually doing what you have described, observing the results, and then changing the
description of what you want the machine to do based on those observations. There is no
bright line where a program is finished and becomes an artifact unto itself. Yes, it is
possible to draw such a line and produce standalone executables in Lisp, just as it is
possible to write interactive programs in **C**. But Lisp was intended to be interactive
(because it was invented to support AI research), whereas C was not (because it was
invented for writing operating systems). **Interactivity is native** to Lisp whereas it is
foreign to C, just as building standalone executable is native to C but foreign to Lisp.

**The key insight is that Lisp programs are not text, they are data structures.**

Of course, there are times when you have no choice but to iterate. Some times you don't
know everything you need to know to produce a finished design and you have to do some
experiments, and the faster you can do them the better off you will be. In cases like
this it is very helpful to have a general mechanism for taking little programs and
composing them to make a bigger program, and the C world has such a mechanism: the pipe.
However, what the C world doesn't have is a standard way of serializing and de-serializing
data. And, in particular, the C world doesn't have a standard way of serializing and
de-serializing hierarchical data. Instead, the C world has a vast array of different
kinds of serialization formats: fixed-width, delimiter-separated, MIME, JSON, ICAL, SGML
and its offspring, HTML and XML, to name but a few. _And those are just serialization_
_formats for data._ If you want to write code, every programming language has its own
syntax with its own idiosyncrasies.

The C ecosystem has spawned the peculiar mindset that thinks that syntax matters. A lot
of mental energy is devoted to syntax design. Tools like **LEX** and **YACC** are widely used.
In the C world, writing parsers is a big part of any programmer's life.

Every now and then someone in the C world gets the bright idea to try to use one of these
data serialization formats to try to represent code. These efforts are short-lived
because code represented in XML or JSON looks absolutely horrible compared to code
represented using a syntax specifically designed to represent code. They conclude that
representing code as data is a Bad Idea and go back to writing parsers.

The reason that code represented as XML or JSON looks horrible is not because representing
code as data is a bad idea, but because XML and JSON are badly designed serialization
formats. And the reason they are badly designed is very simple: too much punctuation.
And, in the case of XML, too much redundancy. The reason Lisp succeeds in representing
code as data where other syntaxes fail is that **S-expression** syntax is a well-designed
serialization format, and the reason it's well designed is that it is minimal. Compare:
```
    XML: <list><item>abc</item><item>pqr</item><item>xyz</item></list>
    JSON: ['abc', 'pqr', 'xyz']
    S-expression: (abc pqr xyz)
```
The horrible bloatedness of XML is obvious even in this simple example.  The difference
between JSON and S-expressions is a little more subtle, but consider: _this is a valid_
_S-expression:_
```
    (for x in foo collect (f x))
```
The JSON equivalent is:
```
    ['for', 'x', 'in', 'foo', 'collect', ['f', 'x']]
```
The difference becomes particularly evident if you try to type those expressions rather
than just look at them. The quotes and commas that seem innocuous enough for
small data structures become an immediately intolerable burden for anything really
complicated.

**The reason that Lisp is so cool and powerful is that the intuition that leads people to**
**try to represent code as data is actually correct.** It is an incredibly powerful lever.
Among other things, it makes writing interpreters and compilers really easy, and so
inventing new languages and writing interpreters and compilers for them becomes as much a
part of day-to-day Lisp programming as writing parsers is business as usual in the C
world. But to make it work you must start with the right syntax for representing code and
data, which means you must start with a minimal syntax for representing code and data,
because anything else will drown you in a sea of commas, quotes and angle brackets.

Which means you have to start with S-expressions, because they are the _minimal syntax_ for
representing hierarchical data. Think about it: to represent hierarchical data you need
two syntactic elements: a token separator and a block delimiter. In S expressions,
whitespace is the token separator and parens are the block delimiters. That's it. You
can't get more minimal than that.

It is worth noting that the reason the parens stick out so much in Lisp is not that Lisp
has more parens than other programming languages, it's that Lisp as _only one_ block
delimiter (parens) and so the parens tend to stick out because there is nothing else.
Other languages have different block delimiters depending on the kind of block being
delimited. The C family, for example, has **()** for argument lists and sub-expressions,
**[]** for arrays, **{}** for code blocks and dictionaries. It also uses commas and
semicolons as block delimiters. If you compare apples and apples, Lisp usually has fewer
block delimiters than C-like languages. _Lisp programmers never have to worry about such
things: if you want to close a_ _block, you type a ")"._ It's always a no brainer, which
leaves Lisp programmers with more mental capacity to focus on the problem they actually
want to solve.

### Packages

_(Notes taken during reading "Practical Common Lisp" by Peter Seibel)_

Do not forget that Common Lisp standard has just a **flat** package system!
And because of that there is no automatic reexport because you do not have parent package.

- Packages are fundamentally a part of the lisp `READER` and not the `EVALUATOR`.
  Packages control how the `READER` maps strings onto symbols.
- In \*PACKAGE\* - current package name is stored
- `a:boo` or `a::foo` are examples of qualified names
- With `:` refer package external symbols, with `::` refer any symbol.
- The operation of getting a symbol from a print name and a current package is called
**interning**. Interning use the following procedure:

```
1. If a symbol with the given print name is registered in the current package, the system
returns that symbol. If not, go to 2.

2. If a symbol with the given print name is an external symbol of the package which the
current package is using, the system returns that symbol. If not, got to 3.

3. The system creates a new symbol with the given print name, registers it as an internal
symbol in the current package, and return the new symbol.
```
Interning is being done when the system reads a form or some data, but it also can be done
using the function ```(intern <character string> <package>*)```.

- **Uninterned** symbols are written with a leading `#:`
- The package in which a symbol is first interned is called the symbol's _home package_.
- A package inherits symbols from other packages by using (`:use` clause) the other
  packages. Only external symbols in the used packages are inherited.
- An existing symbol can be imported into another package by adding it to the package's
  name-to-symbol table.
- Present symbol can be **uninterned** from a package, which causes it to be removed from the
  name-to-symbol table and, if it's a shadowing symbol, from the shadowing list.
- **CL-USER** uses the package **COMMON-LISP**, which exports all the names defined by the
  language standard.

**Tip**: Never work in **CL-USER** package - just create your own.

- Every package has one official name and zero or more _nicknames_.
- The REPL can't start in the COMMON-LISP package because you're not allowed to intern new
  symbols in it. That's way we use CL-USER.
- A package must be defined before you can refer to it. If a package definition refers
  to symbols in other packages (via the `:import-from` and `:shadowing-import-from`
  options) then these symbols must already exist.
- **Macros** (along with any supporting code which the macro functions invoke) must
  be defined before code which refers to them is compiled; and if they're redefined
  then that code must be recompiled. If inline functions aren't already defined then
  the calls to them _won't be inlined_.
- Any code required to evaluate the initial values for global variables must be defined
  before these variables' defining forms (`defvar`, `defparameter` or `defconstant`) are
  loaded.
- Constants are best defined before you refer to them.
- Packages don't provide direct control over who can call what function or access what
  variable. There is no access qualifiers like in Java.
- The **keyword** package is a special package for registering keywords.
  The symbols in this package could be accessed with ```keyword:<symbol-name>``` but by
  convention we use ```:<symbol-name>```.

Unlike other packages, when the system registers a new symbol in the **keyword** package,
the keyword is registered as an external symbol in the **keyword** package and becomes a
_variable_ with a constant value which is the symbol itself. In other words, if the system
evaluates this keyword it gets the keyword itself.
```cl
> :this-is-a-keyword
:this-is-a-keyword

(defun make-keyword (thing)
  (values (intern (string-upcase thing) :keyword)))
```
On page **340** in book you will see how to create variable as keyword runtime:
```cl
(proclaim '(inline variable-p make-variable))
(defun variable-p(x) "Is Χ a variable?" (keywordp x))
;; 'find-package' also searches in package nicknames
(defun make-variable () (gentemp "X" #.(find-package "KEYWORD")))
```

- Instead of keywords, use uninterned symbols, using the `#:` syntax.

``` cl
(defpackage #:ch1
      (:use #:common-lisp))
```

This saves a tiny bit of memory by not interning any symbols in the `KEYWORD` package - the
(keywords resides automatically in KEYWORDS package) symbol can become garbage after
DEFPACKAGE (or the code it expands into) is done with it.
- In the REPL buffer in SLIME you can also pile packages with a REPL shortcut. Type a
comma then `p+`.
- You can import the one symbol you need with an `:import-from` clause in the DEFPACKAGE.
  Another option - rather than listing all the symbols you do want to use in an
  `:import-from clause`, you can instead `:use` the package and then list the names you
  don't want to inherit in a `:shadow` clause.
- `:shadow` clause:

``` cl
(defpackage :ch3
  (:use #:common-lisp
        #:ch1)
  (:import-from :ch2 :address)
  (:shadow :build-index))
```
The `:shadow` clause causes a new symbol named BUILD-INDEX to be created and added
directly to *ch3's* name-to-symbol map. Now if the reader reads the name *BUILD-INDEX*, it
will translate it to the symbol in *ch3's* map, rather than the one that would otherwise be inherited from *ch1*.
The new symbol is also added to a shadowing symbols list that's
part of the *ch3* package, so if you later use another package that also exports a
BUILD-INDEX symbol, the package system will know there's no conflict - that you want the
symbol from *ch3* to be used rather than any other symbols with the same name inherited
from other packages.
- But if you actually want to use one of the inherited symbols, then you need to resolve the ambiguity
with a `:shadowing-import-from` clause.
```cl
(defpackage #:ch4-final
  (:use #:common-lisp
        #:tools
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:paip-aux :debug)) ; use 'debug' from 'paip-aux'
```

- The syntax for `defpackage` allows multiple `export` clauses.
Use this feature to visually group related symbols.

``` cl
(defpackage #:myproject
  (:use #:cl)
  ;; Web stuff
  (:export #:fetch
           #:parse-url
           #:status)
  ;; File utilities
  (:export #:lines
           #:first-line
           #:touch)
  ...)
```
- If packages are confusing, this is the main reason why; they’re not based on objects,
  but on names.
Every package that uses common-lisp imports the name _cons_, because common-lisp includes
a function with that name. But in consequence a **variable** called _cons_ would also be
visible every package that used common-lisp.

- Because packages are used by the reader, a package must be defined before you can LOAD
or COMPILE-FILE a file that contains an IN-PACKAGE expression switching to that package.
- Use ASDF to manage loading and compiling files in the right order.
``` cl
(asdf:defsystem #:study-paip
    :serial t
    :components ((:file "package")
                 (:file "auxfns" :depends-on ("package"))
                 ))
```
- Usually two expressions typed into the toplevel are equivalent to the same two
expressions enclosed within a single progn. Not in this case. If we try saying:

```cl
MINE> (progn (in-package ’common-lisp-user)
             (export ’bar))
>>Error: MINE::BAR is not accessible in COMMON-LISP-USER.
```
we get an error instead. **This happens because the whole progn expression is processed by
read before being evaluated.** When read is called, the current package is mine, so bar is
taken to be mine:bar. It is as if we had asked to export this symbol, instead of
common-lisp-user:bar, from the user package.
- Package names live in a flat namespace - package names are just strings, and different
  packages must have textually distinct names. Java style naming is not bad.
- How to communicate between packages?
```
Here's what the CL HyperSpec tells about:

... use keywords when communicating between programs in different packages ...
... the mechanism for passing keyword parameters in a call uses keywords to name
the corresponding arguments ...

... It is generally best to confine the use of keywords to situations in which there are a
finitely enumerable set of names to be selected between. For example, if there were two
states of a light switch, they might be called **:on** and **:off**.
```
The way packages are defined makes it a **nuisance** to write programs which use symbols as
data. For example, if we define noise as follows:

```cl
(in-package 'other :use 'common-lisp)
(defpackage other
    (:use common-lisp)
    (:export noise))
(defun noise (animal) (case animal
    (dog 'woof)
    (cat 'meow)
    (pig 'oink)))
```
then if we call noise from another package with an unqualified symbol as an argument, it
will usually fall off the end of the case clauses and return nil:

```cl
OTHER> (in-package ’common-lisp-user)
#<Package "COMMON-LISP-USER" 4CD15E>
> (other:noise ’pig)
NIL
```

That’s because what we passed as an argument was _common-lisp-user:pig_ (no offense
intended), while the case key is _other:pig_. To make noise work as one would expect, we
would have to export all six symbols used within it, and import them into any package from
which we intended to call noise.
  In this case, we could evade the problem by using keywords instead of ordinary
symbols. If noise had been defined:

```cl
(defun noise (animal)
    (case animal
        (:dog :woof)
        (:cat :meow)
        (:pig :oink)))
```
Keywords are like gold: universal and self-evaluating. They are visible every- where, and
they never have to be quoted.

- **Gotchas**:

```
CL-USER> (foo)
Ops I forgot it is in 'foolib'.

CL-USER> (use-package :foolib)

Using package 'FOOLIB' results in name conflicts for these symbols: FOO
   [Condition of type PACKAGE-ERROR]
Restarts:
  0: [CONTINUE] Unintern the conflicting symbols from the 'COMMON-LISP-USER' package.
  1: [ABORT] Abort handling SLIME request.
  2: [ABORT] Abort entirely from this (lisp) process.
```

The problem is the first time you called foo, the reader read the name foo and interned it
in CL-USER before the evaluator got hold of it and discovered that this newly interned
symbol isn't the name of a function. This new symbol then conflicts with the symbol of the
same name exported from the FOOLIB package. The solution is to choose: **'0'**

This kind of problem can also occur when loading and compiling files. For instance, if you
defined a package, MY-APP, for code that was going to use functions with names from the
FOOLIB package,but forgot to `:use FOOLIB`, when you compile the files with an `(in-package
:my-app)` in them, the reader will intern new symbols in MY-APP for the names that were
supposed to be read as symbols from FOOLIB. When you try to run the compiled code, you'll
get undefined function errors. If you then try to redefine the MY-APP package to `:use
FOOLIB`, you'll get the conflicting symbols error. **The solution is the same: select the
restart to unintern the conflicting symbols from MY-APP**. You'll then need to recompile
the code in the MY-APP package so it will refer to the inherited names.

- Some Common Lisp implementations, such as SBCL, provide a facility for **"locking"** the
symbols in a particular package so they can be used in defining forms such as DEFUN,
DEFVAR, and DEFCLASS only when their home package is the current package. In others you have
warning about **"redefining BAR, originally defined in...".**

- Error message like: ```The name "CH09" does not designate any package.```
means that there is no ```(defpackage #:ch09 ...``` clause in files that you load.
Check your **.asd** file or for typo error.

### Formatting

_(Notes taken during reading "Practical Common Lisp" by Peter Seibel)_

``` cl
(format t "Price today is ~$" 1.5)
```
- The first parameter to the format function is the destination parameter.
  * **t** is shorthand for the stream \*STANDARD-OUTPUT\*
  * **nil** causes FORMAT to generate its output to a string, which it then returns
  * could be a stream, output is written to the stream

- The second parameter to the format function is a control string, which controls the text formatting.
You can place control sequences into this string to affect the format of the output.
- The FORMATlanguage isn’t Lispy at all - its basic syntax is based on characters, not s-expressions,
and it’s optimized for compactness rather than easy comprehension.
- The format control string contains literal text and formatting directives.
Directives are always introduced with a ~ character.

|  Directive  |  Interpretation                    |
|  ---------  |  --------------                    |
|     ~%      |  new line                          |
|     ~&      |  fresh line                        |
|     ~|      |  page break                        |
|     ~T      |  tab stop                          |
|     ~<      |  justification                     |
|     ~>      |  terminate ~<                      |
|     ~C      |  character                         |
|     ~(      |  case conversion                   |
|     ~)      |  terminate ~(                      |
|     ~D      |  decimal integer                   |
|     ~B      |  binary integer                    |
|     ~O      |  octal integer                     |
|     ~X      |  hexadecimal integer               |
|     ~bR     |  base-b integer                    |
|     ~R      |  spell an integer                  |
|     ~P      |  plural                            |
|     ~F      |  floating point                    |
|     ~E      |  scientific notation               |
|     ~G      |  ~F or ~E, depending upon magnitude|
|     ~$      |  monetary                          |
|     ~A      |  legibly, without escapes          |
|     ~S      |  READably, with escapes            |
|     ~~      |  ~                                 |

#### FORMAT directives

- All directives start with a *tilde* (~) and end with a single character that identifies the directive.
- You can write the character in either upper- or lowercase.
- Some directives take prefix parameters, which are written immediately following the tilde,
separated by commas, and used to control things such as how many digits to print after the decimal point
when printing a floating-point number.
- The values of prefix parameters are either numbers, written in decimal, or characters,
written as a single quote followed by the desired character.
- The value of a prefix parameter can also be derived from the format arguments in two ways:
  * Prefix parameter of **v** causes FORMAT to consume one format argument
``` cl
(format t "~v$" 3 pi) ;=> 3.142
```
- You can break up long format control strings with ~ at the end of a line.

``` cl
(format t "It was the best of times, ~
             it was the worst of times.")
It was the best of times, it was the worst of times.
NIL
```
  * Prefix parameter of **#** will be evaluated as the number of remaining format arguments
``` cl
(format t "~#$" pi) ; the same as (format t "~1$" pi)
```
- If you want to specify one parameter but not the ones before it, you must include a
comma for each unspecified parameter.
``` cl
(format t "~,5f" pi) ; second parameter controls the number of decimal places: 3.14159
```
- You can also modify the behavior of some directives with **colon** and **@-sign** modifiers,
which are placed after any prefix parameters and before the directive's identifying
character. These modifiers change the behavior of the directive in small ways.

#### Basic Formatting

- Directive is **~A**, which consumes one format argument of any type and outputs it in
_aesthetic_ (human-readable) form.
- A closely related directive, **~S**, likewise consumes one format argument of any type and
outputs it. However, **~S** tries to generate output that can be read back in with *READ*.
- With a colon modifier, both the **~A** and **~S** directives emit *NIL* as *()* rather than *NIL*.
- The other two most frequently used directives are **~%**, which emits a newline, and **~&**,
which emits a fresh line. The difference between the two is that **~%** always emits a
newline, while **~&** emits one only if it’s not already at the beginning of a line.


#### Character and Integer Directives

- **~C** directive is used to emit characters.
- With a colon modifier, **~:C** outputs nonprinting characters such as space, tab, and
newline by name.
``` cl
(format t "Syntax error. Unexpected character: ~:c" char)
;; => Syntax error. Unexpected character: Space
```
- With the at-sign modifier, **~@C** will emit the character in Lisp’s literal character
syntax.
``` cl
(format t "~@c~%" #\a) ;=> #\a
```
- Five closely related directives format integer values: **~D**, **~X**, **~O**, **~B**, and **~R**. The most
frequently used is the **~D** directive, which outputs integers in base 10.
``` cl
(format nil "~d" 1000000) ;=> "1000000"
(format nil "~:d" 1000000) ;=> "1,000,000"
(format nil "~@d" 1000000) ;=> "+1000000"
(format nil "~:@d" 1000000) ;=> "+1,000,000"
```
- The first prefix parameter can specify a minimum width for the output, and the second
parameter can specify a padding character to use. The default padding character is space,
and padding is always inserted before the number itself.
``` cl
(format nil "~4,'0d-~2,'0d-~2,'0d" 2005 6 10) ;=> "2005-06-10"
```

#### Floating-Point Directives

- Four directives format floating-point values: **~F**, **~E**, **~G**, and **~$**.
- The **~F** directive emits its argument, which should be a number, in decimal format,
possibly controlling the number of digits after the decimal point. The **~F** directive
is, however, allowed to use computerized scientific notation if the number is sufficiently
large or small.
- The **~$**, or monetary - equivalent to **~,2F**, directive is similar to **~F** but a bit simpler.
- To modify the number of digits printed after the decimal point, you use the first
parameter, while the second parameter controls the minimum number of digits to print
before the decimal point.
``` cl
(format nil "~2,4$" pi) ;=> "0003.14"
```

#### English-Language Directives

- The **~R** directive when used with no base specified, prints numbers as English words
or _Roman numerals_.
``` cl
(format nil "~r" 1234) ;=> "one thousand two hundred thirty-four"
(format nil "~:r" 1234) ;=> "one thousand two hundred thirty-fourth"
(format nil "~@r" 1234) ;=> "MCCXXXIV"
```
- Note that for numbers too large to be represented in the given form, **~R** behaves like **~D**.
- To help you generate messages with words properly pluralized, FORMAT provides the **~P**
directive, which simply emits an **s** unless the corresponding argument is 1.
``` cl
(format nil "file~p" 1) ;=> "file"
(format nil "file~p" 10) ;=> "files"
```
- **~[** is more flexible that **~P**
- **~(** allows you to control the case of text in the output.
- Each **~(** is paired with a **~)**, and all the output generated by the portion of the
control string between the two markers will be converted to all lowercase.
- You can modify **~(** with an at sign to make it capitalize the first word in a section
of text, with a colon to make it to capitalize all words, and with both modifiers to
convert all text to uppercase.
``` cl
(format nil "~(~a~)" "tHe Quick BROWN foX") ;=> "the quick brown fox"
(format nil "~@(~a~)" "tHe Quick BROWN foX") ;=> "The quick brown fox"
(format nil "~:(~a~)" "tHe Quick BROWN foX") ;=> "The Quick Brown Fox"
(format nil "~:@(~a~)" "tHe Quick BROWN foX") ;=> "THE QUICK BROWN FOX"
```

#### Conditional Formatting

- FORMAT provides several directives that implement simple control constructs within the
control string.
- the conditional directive **~[**. This directive is closed by a corresponding **~]**,
and in between are a number of clauses separated by **~;**
- With no modifiers or parameters, the clause is selected by numeric index
- The **~[** directive consumes a format argument, which should be a number, and takes the
_nth_ (zero-based) clause where _N_ is the value of the argument.
``` cl
(format nil "~[cero~;uno~;dos~]" 0) ;=> "cero"
(format nil "~[cero~;uno~;dos~]" 1) ;=> "uno"
(format nil "~[cero~;uno~;dos~]" 2) ;=> "dos"
(format nil "~[cero~;uno~;dos~]" 3) '=> ""
```

- However, if the last clause separator is **~:;** instead of **~;**, then the last clause
serves as a default clause.
``` cl
(format nil "~[cero~;uno~;dos~:;mucho~]" 100) ;=> "mucho"
```
- It’s also possible to specify the clause to be selected using a prefix parameter.
``` cl
(format nil "~0[cero~;uno~;dos~:;mucho~]" 100) ;=> "cero"
```
- **#** used as a prefix parameter means the number of arguments remaining to be processed.
``` cl
(defparameter *list-etc*
          "~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")
(format nil *list-etc*) ;=> "NONE." using first clause
(format nil *list-etc* 'a 'b 'c 'd 'e) ;=> "A, B, C, etc." using second clause
```
- With a _colon_ modifier, the **~[** can contain only two clauses; the directive consumes
a single argument and processes the first clause if the argument is NIL and the second
clause is otherwise.

``` cl
;; Note that either clause can be empty, but the directive must contain a ~;
(format t "~:[FAIL~;pass~]" test-result)
```

#### Iteration

- **~{** directive tells FORMAT to iterate over the elements of a list or over the
implicit list of the format arguments.

- With no modifiers, **~{** consumes one format argument, which must be a list.
- **~{** directive is always paired with a closing **~}**
- The text between the two markers is processed as a control string, which draws its
arguments from the list consumed by the **~{** directive.
- String **"~a, "**,repeating until all the elements of the list have been consumed.
``` cl
(format nil "~{~a, ~}" (list 1 2 3)) ;=> "1, 2, 3, " Did you spot annoying last comma?
```
- The **~^** causes the iteration to stop immediately, without processing the rest of the
control string, when no elements remain in the list.
``` cl
(format nil "~{~a~^, ~}" (list 1 2 3)) ;=> "1, 2, 3"
```
- With an **@-sign** modifier, **~{** processes the remaining format arguments as a list.
``` cl
(format nil "~@{~a~^, ~}" 1 2 3) ;=> "1, 2, 3"
```
- Within the body of a **~{...~}**, the special prefix parameter **#** refers to the
number of items _remaining_ to be processed in the list rather than the number of remaining
format arguments.
``` cl
;; ", and 3" is added by ~#[~;, and ~:;, ~]
(format nil "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3)) ;=> "1, 2, and 3"
```
- When **~@{** is nested inside another **~{** or **~@{** directive - it iterates over
whatever items _remain_ in the list being iterated over by the outer **~{**.

``` cl
(defparameter *english-list*
  "~{~#[<empty>~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}")

(format nil *english-list* '()) ;=> "<empty>"
(format nil *english-list* '(1 2 3)) ;=> "1, 2, and 3"
(format nil *english-list* '(1 2 3 4)) ;=> "1, 2, 3, and 4"
```

#### Hop, Skip, Jump

- The `~*` directive allows you to jump around in the list of format arguments.
- Without modifiers, it simply skips the next argument, consuming it _without emitting
anything_.
- More often, it’s used with a colon modifier, which causes it to move backward, allowing
the same argument to be used a _second_ time.
``` cl
(format nil "~r ~:*(~d)" 1) ;=> "one (1)"
```
- Within an **~{** directive, `~*` _skips_ or _backs up_ over the items in the list. For
instance, you could print only the keys of a plist like this:
``` cl
(format nil "~{~s~*~^ ~}" '(:a 10 :b 20)) ;=> ":A :B"
```

### Macros

_(Notes taken during reading "Practical Common Lisp" by Peter Seibel)_

- Macros are simply programs which change/generate code, but this happens during compile
time.
- Functions evaluate all of their arguments before entering the body of the function.
Macros don't evaluate any of their arguments unless you tell it so.
- The return value of a macro is a Lips list that's treated as code.
- Macros allow you to define operators that are implemented by transformation.

```
SBCL compiles all functions in one file at one go. Suppose a file contains

- a definition of a macro,
- a definition of a function that is called by the macro, and
- a use of the macro.  Now, the macro cannot use the function before it has been
compiled. Thus, the use of the macro cannot be compiled...

Possible solutions

- Put macros (with help functions) and their uses in different files and make sure that
  the macro definitions are compiled before their uses (manually or using asdf).
- Define help functions as local functions using labels.
```

#### Macro Expansion Time vs. Runtime

- When you write macros, you're writing programs that will be used by the compiler to
generate the code that will then be compiled.
- The time when macros run is called macro expansion time; this is distinct from runtime,
  when regular code, including the code generated by macros, runs.(Lisp blurs the
  distinction between run-time and compile-time.)
- It’s important to keep this distinction firmly in mind because code running at macro
  expansion time runs in a very different environment than code running at
  runtime. Namely, at macro expansion time, there’s no way to access the data that will
  exist at runtime.
- Code running at macro expansion time can deal only with the data that’s inherent in the
  source code. For instance, suppose the following source code appears somewhere in a
  program:
``` cl
(defun foo (x)
  (when (> x 10) (print 'big)))
```
Normally you'd think of **x** as a variable that will hold the argument passed in a call
to foo. But at macro expansion time, such as when the compiler is running the WHEN macro,
the only data available is the source code.  Instead,the values the compiler passes to
WHEN are the Lisp lists representing the source code, namely , `(> x 10)` and `(print'big)`.
Suppose that WHEN is defined, with something like the following macro:
``` cl
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```
When the code in `foo` is compiled, the WHEN macro will be run with those two forms as
arguments. The parameter condition will be bound to the form `(> x 10)`, and the form
`(print 'big)` will be collected into _a list_ that will become the value of the `&rest`
body parameter._Compiler builds the expression specified by the definition, then compiles
the expression in the place of the original macro call_.
- The job of the macro is still to produce code that will do something rather than to do
  anything directly.

#### DEFMACRO

- The basic skeleton of a DEFMACRO is quite similar to the skeleton of a DEFUN.
``` cl
(defmacro name (parameter*)
  "Optional documentation string."
   body-form*)
```
- The job of a macro is to translate a macro form - in other words, a Lisp form whose first
  element is the name of the macro - into code that does a particular thing.
- How to write MACRO guideline:

```
1. Write a sample call to the macro and the code it should expand into, or vice versa.
2. Write code that generates the hand written expansion from the arguments int he sample call.
3. Make sure the macro abstraction doesn't "leak"
```

#### A Sample Macro: do-primes

**The task**

```
Write a macro do-primes that provides a looping construct similar to DOTIMES and DOLIST
except that instead of iterating over integers or elements of a list, it iterates over
successive prime numbers.
```
Let's start writing using guideline.

_1. Write a sample call to the macro and the code it should expand into, or vice versa._

First, you’ll need two _utility functions_, one to test whether a given number is prime
and another that returns the next prime number greater or equal to its argument.

``` cl
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```

Without the `do-primes` macro, you could write such a loop with **DO** this:
``` cl
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))
```
Here is the _example_ of usage:
``` cl
(do-primes (p 0 19)
  (format t "~d " p))
```
The first argument to the `do-primes` call is a list containing the name of the loop
variable, **p**; the lower bound, 0; and the upper bound, 19.

_2. Write code that generates the hand written expansion from the arguments int he sample call._

Since the arguments passed to a macro are Lisp objects representing the source code of the
macro call, the first step in any macro is to extract whatever parts of those objects are
needed to compute the expansion.

You could define `do-primes` with two parameters, one to hold the list and a `&rest`
parameter to hold the body forms, and then take apart the list by hand, something like
this:
``` cl
;; first attempt
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range)) ; prepare
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))) ; expansion
         ((> ,var ,end))
       ,@body)))
```
TIP: Do you know that ``` `(,x ,y)``` could be written as ```(cons x (cons y ()))```?

However, you don't need to take apart ```var-and-range``` "by hand" because macro parameter
lists are what are called **destructuring** parameter lists. Destructuring, as the name
suggests, involves taking apart a structure - in this case the list structure of the forms
passed to a macro.

Within a destructuring parameter list, a simple parameter name can be replaced with a
nested parameter list. The parameters in the nested parameter list will take their values
from the elements of the expression that would have been bound to the parameter the list
replaced. For instance, you can replace ```var-and-range``` with a list ```(var start
end)```, and the three elements of the list will automatically be destructured into those
three parameters. Hm, but _what is_ ```body```?

It tells the macro expander "Give me all remaining expressions in the macro in a list."
More strictly it holds a _list of forms_(functions logic implementation) that make up the
body of the macro. Here is a good example that clearly points where is the `body`:

```cl
(defmacro define (name-and-parameters &rest body)
  `(defun ,(first name-and-parameters) ,(rest name-and-parameters)
    ,@body))
```

Now better version of ```do-primes```:
``` cl
;; improved attempt
(defmacro do-primes ((var start end) &body body)     ;; use &body not &rest
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
```
In addition to being more concise, _destructuring_ parameter lists also give you automatic
error checking - with ```do-primes``` defined this way, Lisp will be able to detect a call whose
first argument isn't a three-element list and will give you a meaningful error message
just as if you had called a function with too few or too many arguments. Also, in
development environments such as SLIME that indicate what arguments are expected as soon
as you type the name of a function or macro, if you use a destructuring parameter list,
the environment will be able to tell you more specifically the syntax of the macro call.

NOTE: ```&rest``` and ```&body``` do the same thing. ```&body``` is a hint to the system
and  the human reader of the code that the remaining arguments are something like a
function body.

```&rest``` mainly communicates _"a list of objects, mostly of similar type"_, while
```&body``` mainly communicates _"forms to be evaluated in the context set up by the macro"._

``` cl
(do-primes var-and-range &rest body)   ; for first attempt
(do-primes (var start end) &body body) ; for latest
```

#### Generating the Expansion

Backquoted expression is similar to a quoted expression except you can "unquote"
particular subexpressions by preceding them with a comma, possibly followed by an **@**
sign. Without an **@** sign, the comma causes the value of the subexpression to be
included as is. With an at sign (**@**), the value - which must be a list - is
"spliced"(append) into the enclosing list. Another useful way to think about the backquote
syntax is as a particularly concise way of writing code that generates lists.

Here is some examples:

```
Example 1:

Backquote Syntax     Equivalent List-Building Code  Result
`(a (+ 1 2) c)       (list 'a '(+ 1 2) 'c)          (a (+ 1 2) c)
`(a ,(+ 1 2) c)      (list 'a (+ 1 2) 'c)           (a 3 c)
`(a (list 1 2) c)    (list 'a '(list 1 2) 'c)       (a (list 1 2) c)
`(a ,(list 1 2) c)   (list 'a (list 1 2) 'c)        (a (1 2) c)
`(a ,@(list 1 2) c)                                 (a 1 2 c) ; @ interpolates lists.

Example 2:
Suppose that B is bound to (1 2)

Backquote Syntax     Result
`(a b c)             (a b c)
`(a ,b c)            (a (1 2) c)
`(a ,@b c)           (a 1 2 c)
`(a b ,b ,@b c)      (a b (1 2) 1 2 c)
```

**@** will place the elements of the list _directly_ inside in the expansion.
Do you remember what is `body`? It holds a **list** of forms that make up the body of the
macro! We have to traverse this list and insert very element in macros body. That's why we
always see `,@body`. Let's illustrate with an example:

```cl
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
    ,@body))

;; consider the following use of our let1 macro:
(let1 foo (+ 2 3)
  (princ "Lisp is awesome!")
  (* foo foo))

;; body <==> ((princ "Lisp is awesome!") (* foo foo)), so ,@body place them

```
**@** is useful in macros that have `&rest` parameters representing, a body of a code.

Let's test it by hand

``` cl
(do-primes (p 0 19) (format t "~d " p)) ;=> 2 3 5 7 11 13 17 19
```

and using `macroexpand-1`

``` cl
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
;=> (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))) ((> P 19)) (FORMAT T "~d " P))
```

**NOTE**: If the macro expansion is shown all on one line, it's probably because the variable
\*PRINT-PRETTY\* is NIL. If it is, evaluating `(setf *print-pretty* t)` should make the
macro expansion easier to read.

#### Plugging the Leaks

_3. Make sure the macro abstraction doesn't "leak"_

- It evaluates the end subform too many times.

Suppose you were to call `do-primes` with, instead of a literal number such as 19, an
expression such as `(random 100)` in the end position.

``` cl
(do-primes (p 0 (random 100))
  (format t "~d " p))

(macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P (RANDOM 100)))
  (FORMAT T "~d " P))
```
Instead of looping until **p** is greater than an initially chosen random number, this
loop will iterate until it happens to draw a random number less than or equal to the
current value of **p**.

**TIP**: Always test your macros with expressions.

You should try to observe the Principle of Least Astonishment when implementing macros.
Since `do-primes` is built on the model of the standard macros, DOTIMES and DOLIST,
neither of which causes any of the forms except those in the body to be evaluated more
than once, most programmers will expect `do-primes` to behave similarly.

You can fix the multiple evaluation easily enough; you just need to generate code that
evaluates `end` once and saves the value in a variable to be used later.

``` cl
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
     (,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ending-value))
,@body))
```
**ATTENTION**: This fix introduces two new leaks to the macro abstraction.

- One new leak is similar to the multiple-evaluation leak you just fixed. Because the
  initialization forms for variables in a DO loop are evaluated in the order the variables
  are defined, when the macro expansion is evaluated, the expression passed as end will be
  evaluated before the expression passed as start, opposite to the order they appear in
  the macro call. This leak doesn't cause any problems when start and end are literal
  values like 0 and 19. But when they're forms that can have side effects, evaluating them
  out of order. To fix it just swap the order:

``` cl
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end)) ; the new place of end
       ((> ,var ending-value))
,@body))
```

- The last leak you need to plug was created by using the variable name `ending-value`.

What happen if you call `do-primes` like this:

``` cl
(do-primes (ending-value 0 10)
  (print ending-value))
```
Again, MACROEXPAND-1 can show you the problem.

``` cl
(do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
     (ending-value 10))
    ((> ending-value ending-value)) ; <- the problem is here
  (print ending-value))
```
The code will loop forever since ending - value will never be greater than itself!
Clearly, what you need to patch this leak is a symbol that will never be used outside the
code generated by the macro. The function **GENSYM** returns a unique symbol each time
it's called. This is a symbol that has never been read by the Lisp reader and never will
be because it isn't interned in any package.

``` cl
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
```

Note that the code that calls **GENSYM** isn't part of the expansion; it runs as part of
the macro expander and thus creates a new symbol each time the macro is expanded.

**TIP**: There's no real downside to using a gensymed name just to be safe.

It's actually fairly simple to write macros if you follow these rules of thumb:
```
1 Unless there's a particular reason to do otherwise, include any subforms in the
expansion in positions that will be evaluated in the same order as the subforms appear in
the macro call.

2 Unless there's a particular reason to do otherwise, make sure subforms are evaluated
only once by creating a variable in the expansion to hold the value of evaluating the
argument form and then using that variable anywhere else the value is needed in the
expansion.

3 Use GENSYM at macroexpansion time to create variable names used in the expansion.
```

_That's it!_
