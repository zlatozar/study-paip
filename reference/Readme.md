## Contents

- [Chapter-1](#chapter-1)
    - [append](#append)
    - [length](#length)
    - [mapcar](#mapcar)
    - [member](#member)
    - [apply](#apply)
    - [null](#null)
    - [numberp](#numberp)
    - [atom](#atom)
- [Chapter-2](#chapter-2)
    - [elt](#elt)
    - [random](#random)
    - [assoc](#assoc)
    - [list](#list)
    - [make-list](#make-list)
- [Chapter-3](#chapter-3)
    - [flet](#flet)
    - [labels](#labels)
    - [incf](#incf)
    - [remove](#remove)
    - [every](#every)
    - [some](#some)
    - [mapc](#mapc)
    - [count](#count)
    - [delete](#delete)
    - [find](#find)
    - [position](#position)
    - [reduce](#reduce)
    - [substitute](#substitute)
    - [nth](#nth)
    - [reverse](#reverse)
    - [subseq](#subseq)
    - [aref](#aref)
    - [char](#char)
    - [bit](#bit)
    - [concatenate](#concatenate)
    - [eq](#eq)
    - [equal](#equal)
    - [equalp](#equalp)
    - [copy-tree](#copytree)
    - [tree-equal](#treeequal)
    - [subst](#subst)
    - [sublis](#sublis)
    - [rem](#rem)
    - [mod](#mod)
    - [intersection](#intersection)
    - [nintersection](#nintersection)
    - [union](#union)
    - [nunion](#nunion)
    - [set-difference](#setdifference)
    - [nset-difference](#nsetdifference)
    - [subsetp](#subsetp)
    - [check-type](#checktype)
    - [print](#print)
    - [print1](#print)
    - [printc](#printc)
    - [read](#read)
    - [cerror](#cerror)
    - [nconc](#nconc)
- [Chapter-4](#chapter-4)
    - [push](#push)
    - [fresh-line](#fresh-line)
    - [mapcan](#mapcan)
    - [consp](#consp)
    - [listp](#listp)
    - [copy-list](#copy-list)
- [Chapter-5](#chapter-5)
    - [read-from-string](#read-from-string)
- [Chapter-6](#chapter-6)
    - [handler-case](#handler-case)
    - [progv](#progv)
    - [adjoin](#adjoin)
    - [merge](#merge)
- [Chapter-9](#chapter-9)
    - [sharpsign](#sharpsign)
    - [multiple-value-bind](#multiple-value-bind)
    - [prog1](#prog1)
    - [multiple-value-prog1](#multiple-value-prog1)
    - [pop](#pop)
    - [set-difference](#set-difference)
    - [time](#time)
    - [proclaim](#proclaim)
    - [unwind-protect](#unwind-protect)
- [Chapter-10](#chapter-10)
    - [intern](#intern)
    - [the](#the)
    - [svref](#svref)
    - [simple-vector](#simple-vector)
    - [simple-array](#simple-array)
- [Chapter-11](#chapter-11)
    - [get](#get)
    - [pushnew](#pushnew)
- [Chapter-12](#chapter-12)
    - [values](#values)
    - [catch](#catch)
    - [terpri](#terpri)
- [Koans](#koans)
- [Misc](#misc)
    - [getf](#getf)

## Chapter-1

### append

(**append** _list\*_) => list

Argument description:
- _list_ -  lists to be concatenated

APPEND function concatenates list arguments into one list. Resulting list is
shallow copy of specified lists except for the **last** which is directly shared.
The command to APPEND would be "link the following lists together". If an empty list, NIL
is an argument, it is not included in the result.

See also MAPCAN, CONS, LIST, LIST\*.

``` cl
(append) ;=> NIL
(append '(I) nil '(do not believe it)) ;=> (I DO NOT BELIEVE IT)
(append '(1 2 3)) ;=> (1 2 3)
(append '(1 2 3) '(4 5 6)) ;=> (1 2 3 4 5 6)
(append '(1 2 3) '(4 5 6) '(7 8 9)) ;=> (1 2 3 4 5 6 7 8 9)
(let ((x '(tail list))) (eq x (cddr (append '(front list) x)))) ;=> T
```

### length

(**length** _sequence_) => N

Argument description:
- _sequence_ - a proper sequence
- _N_        - a non-negative integer

Returns the number of elements in SEQUENCE. Only the top level is counted. Elements in
sub-lists are disregarded. If SEQUENCE is a vector with a fill pointer, the active length
as specified by the fill pointer is returned.

See also LIST-LENGTH

``` cl
(length nil) ;=> 0
(length "abc") ;=> 3
(setq str (make-array '(3) :element-type 'character
                           :initial-contents "abc"
                           :fill-pointer t)) ;=> "abc"
(length str) ;=> 3
(setf (fill-pointer str) 2) ;=> 2
(length str) ;=> 2
```

### mapcar (aka transform)

(**mapcar** _fn_ _list+_) => list

Argument description:
- _fn_    - function that takes as many arguments as there are lists
- _list+_ - lists which elements are processed in **parallel**

MAPCAR (closest to Scheme MAP) applies function FN to elements of lists with same index.
Each application result is put into resulting list. Length of resulting list is the length
of the shortest list argument.

See also MAPCAN and MAPPEND(from the book)

``` cl
(mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) ;=> (11 12 13 14)
(mapcar #'round '(1.3 2.7 3.4 4.5)) ;=> (1 3 3 4)
(mapcar #'list '(123 symbol "string" 345) '(1 2 3))
                         ;=> ((123 1) (SYMBOL 2) ("string" 3))
(mapcar #'* '(3 4 5) '(4 5 6)) ;=> (12 20 30)
```

### member

(**member** _item_ _list_ :test :key) => tail or NIL

Argument description:
- _item_   - an item to be found
- _list_   - a list to be searched
- _test_   - function key and item comparison
- _key_    - function for extracting value before test

If the LISP expression was found, the tail of the list starting with the LISP expression
is returned. In case the expression could not be found, the function returns NIL.
MEMBER will only search the top level of the list, so with nested lists it will not go any
deeper than the top level.

See also MEMBER-IF, POSITION, POSITION-IF, FIND, FIND-IF and FIND-ALL(from the book).

``` cl
(member 'i '(a i o u)) ;=> (I O U)
(member 2 '(0 1 0 0 0 1 0)) ;=> NIL
(member #\h '(#\H #\o #\l #\a)) ;=> NIL
(member #\h '(#\H #\o #\l #\a) :test #'char-equal) ;=> (#\H #\o #\l #\a)
(member #\h '(#\H #\o #\l #\a) :key #'char-downcase) ;=> (#\H #\o #\l #\a)
```

### apply

(**apply** _fn_ _args\+_) => value

Argument description:
- _fn_    - a function designator
- _args_  - call arguments

Common Lisp provides two functions for invoking a function through a function object:
FUNCALL and APPLY. They differ only in how they obtain the arguments to pass to the
function. Loosely say:<br/>
```(apply fn '(1 2 3 4 5))``` is ```(fn 1 2 3 4 5)```<br/>

APPLY takes a function, plus one more argument which must **evaluate to a list.**

It so happens that apply can do one additional trick. Alternatively, APPLY can look like
this: ```(apply fn arg1 arg2 ... list-arg)```<br/>
The last argument must evaluate to a list. Here, APPLY builds a list before passing it to
the function. This list is built by taking each of the _arg1_, _arg2_, arguments and
concatenating their values to the front of the list returned by _list-arg_.

Argument list is constructed as:
``` cl
(append (all but list-arg) (first (last list-arg)))
```
For example: <br/>
```(apply #'+ 1 2 3 '(4 5 6))```, the concatenation results in the list '(1 2 3 4 5 6). Thus ```(apply #'+ 1 2 3 '(4 5 6))```<br/>
is the same thing as ```(apply #'+ '(1 2 3 4 5 6))``` which is the same thing as ```(apply #'+ 1 2 3 4 5 6 '())```<br/>
which of course is the same thing as ```(apply #'+ 1 2 3 4 5 6 nil)```

Note that there is limitation of maximal number of arguments, see CALL-ARGUMENTS-LIMIT constant.

See also FUNCALL, LAMBDA and EVAL.

``` cl
(apply #'+ 1 2 3 '(4 5 6)) ;=> 21
(apply 'cons '((+ 2 3) 4)) ;=> ((+ 2 3) . 4)
```

### null

(**null** object) => T or NIL

Argument description:
- _object_ - an object

NULL function returns true if the argument is NIL, otherwise it returns
false. NULL is identical to NOT, but used in conjunction with list processing
unlike NOT which is used in boolean logic processing.

``` cl
(null nil) ;=> T
(null t) ;=> NIL
(null '()) ;=> T

(null '(1 2 3)) ;=> NIL
(null 234.4) ;=> NIL
(null "lisp") ;=> NIL
```

### numberp

(**numberp** _object_) => T or NIL

Argument description:
- _object_ - an object

Returns true if OBJECT is of type NUMBER; otherwise, returns false.

``` cl
(numberp 12) ;=> T
(numberp nil) ;=> NIL
(numberp (cons 1 2)) ;=> NIL

;; Numbers
9999999999999999999999 ; integers
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratios
#C(1 2)                ; complex numbers
```

### atom

(**atom** _object_) => T or NIL

Argument description:
- _object_ -  an object

ATOM function returns true if the argument is not a cons cell, otherwise it
returns false.

See CONS and LIST.

``` cl
(atom nil) ;=> T
(atom 'some-symbol) ;=> T
(atom 3) ;=> T
(atom "moo") ;=> T
(atom (cons 1 2)) ;=> NIL
(atom '(1 . 2)) ;=> NIL
(atom '(1 2 3 4)) ;=> NIL
(atom (list 1 2 3 4)) ;=> NIL
```

## Chapter-2

### elt

(**elt** _sequence_ _index_) => element

Argument description:
- _sequence_ - a sequence
- _index_    - valid sequence index

ELT function accesses specified elements of sequences. The index is counted from
**zero**. Accessing out-of-bounds indices signals condition, or causes crash and/or
undefined behavior, depending on compilation safety mode. **Unlike AREF, ELT works
on lists too.**

ELT may by used with conjunction of SETF.

``` cl
(elt "hola" 0) ;=> #\h
(elt "hola" 3) ;=> #\a
(elt #(5 3 6 8) 1) ;=> 3
(elt '(5 3 6 8) 1) ;=> 3
(let ((a (list 1 2 3 4))) (setf (elt a 1) 'x) a) ;=> (1 X 3 4)
(let ((a (copy-seq "hola"))) (setf (elt a 1) #\O) a) ;=> "hOla"
```

### random

### elt

(**random** _limit_ _random-state?_) => numeric value

Argument description:
- _sequence_ - positive number, integer or real
- _index_    - object representing random state. Default is ****random-state**** global
               variable.

RANDOM function generates random numbers. For integer argument N, result is integer
between zero (including) and N (excluding).

### assoc

(**assoc** _item_ _alist_ :key :test) => cons cell or NIL

Argument description:
- _item_  - a key object
- _alist_ - **alist** - list of cons cell with key-value pairs
- _key_   - function for extracting key **before** test
- _test_  - function key and item comparison

ASSOC function searches supplied list for cons cell that have item as car
part. Return value is the cell with key-value pair which key matched testing
conditions, otherwise NIL. Think of it as _"give me the value for a key"_.
**Default comparison operator is EQL.**

Associative list, or for short alist, is a list with key-value pairs in cons
cells. That is:
``` cl
((key1 . value1) (key2 . value2) ...)
```

``` cl
(assoc 'a '((a . 1) (b . 2) (c . 3))) ;=> (A . 1)
(assoc 'x '((a . 1) (b . 2) (c . 3))) ;=> NIL
(assoc 'b '((a . 1) (b . 2) (c . 3) (b . 4))) ;=> (B . 2)
(assoc "b" '(("a" . 1) ("b" . 2))) ;=> NIL
(assoc "b" '(("a" . 1) ("b" . 2)) :test #'equal) ;=> ("b" . 2)
(assoc 7 '((6 . a) (9 . b)) :key #'1+) ;=> (6 . A)
(assoc 5 nil) ;=> NIL
```

### list

(**list** _list\*_) => list

Argument description:
- _list_ - list of objects

LIST function makes new list from arguments. If the empty list, NIL is one of the
arguments, it will be listed in the result. In simple terms, this function will "list the
following arguments" e.g. ```(list nil '(paip)) ;=> (NIL (PAIP))```. Here is an
interesting example:
```cl
(list 'study-paip) ;=> (study-paip)
```
Example shows how a list can be constructed from an atomic expression and with only a
single element (the atom).

See also LISTP, LIST\*

``` cl
(list 1 2 3) ;=> (1 2 3)
(list 'a '(b c)) ;=> (A (B C)) ; (cons 'a (cons '(b c) nil))
(equal (list 1 'a 3) (cons 1 (cons 'a (cons 3 nil)))) ;=> T

(list 'a #c(1 2) "moo") ;=> (A #C(1 2) "moo")

(list) ;=> NIL
(eq (list) '()) ;=> T

;; Be careful
(equal (list 1 'a 3) '(1 . (a . (3 . nil)))) ;=> T
```

### make-list

(**make-list** _size_ :initial-element) => list

Argument description:
- _size_ - list size

This creates and returns a list containing _size_ elements, each of which
is initialized to the `:initial-element` argument (which defaults to nil).

_size_ should be a non-negative integer.

``` cl
(make-list 5) ;=> (nil nil nil nil nil)
(make-list 3 :initial-element 'rah) ;=> (rah rah rah)
```

## Chapter-3

FLET and LABELS let you define a function that can be referred to only within the scope of
the FLET or LABELS form. These special operators are handy when you need a local function
that's a bit too complex to define inline as a LAMBDA expression or that you need to use
more than once.

### flet

Bindings are not recursive and cannot refer to each other. Each binding contains function
name, arguments, and function body.

```cl
(flet (function-definition*)
      body-form*)
```

Example:
```cl
(flet ((sin2x (x) (sin (* 2 x)))
       (cos2x (x) (cos (* 2 x))))
 (+ (sin2x 0.2) (cos2x 0.2))) ;=> 1.3104793
```

### labels

Bindings can be recursive and can refer to each other.

(**labels** _(function-definition+)_ _body-of-labels_) ;=> an object

Example:
```cl
(labels ((fact2x (x) (fact (* 2 x)))
         (fact (x) (if (< x 2) 1 (* x (fact (1- x))))))
  (fact2x 3)) ;=> 720
```

### incf

(**incf** _place_ _increment?_) => numeric value

Argument description:
- _place_     -  place with numeric value
- _increment_ -  numeric value

INCF macro modifies a place with numeric value. Its value is incremented by
increment number. **Default increment is 1.**

See also 1+, DECF and 1-

``` cl
(let ((a 10)) (incf a) a) ;=> 11
(let ((a 10)) (incf a 2.3) a) ;=> 12.3
(let ((a 10)) (incf a -2.3) a) ;=> 7.7
(let ((a (list 10 11 12 13))) (incf (elt a 2) 2.3) a) ;=> (10 11 14.3 13)
```

### remove

(**remove** _item_ _seq_ :from-end :test :test-not :start :end :count :key) => sequence

Argument description:
- _item_     - an object
- _seq_      - a sequence
- _from-end_ - boolean specifying processing direction
- _test_     - equality test
- _test-not_ - non-equality test
- _start_    - bounding index, **default 0**
- _end_      -  bounding index, **default nil**
- _count_    - integer for how many elements to remove, or nil
- _key_      - function of one argument

REMOVE make new sequence of the same type that has some elements removed.
COUNT may limit the number of removed elements.

See also REMOVE-IF, DELETE, DELETE-IF, SUBSEQ, and REMOVE-DUPLICATES.

``` cl
(remove 10 '(1 2 3 10) :count 1 :test #'eq) ;=> (1 2 3)
(remove #\s "Sample string sequence") ;=> "Sample tring equence"
(remove #\s "Sample string sequence" :count 1) ;=> "Sample tring sequence"
(remove #\s "Sample string sequence" :test #'char-equal) ;=> "ample tring equence"
(remove nil '(1 2 nil 4 nil 6)) ;=> (1 2 4 6)
```

### every
### some

(**every** _predicate_ _sequences+_) => T or NIL<br/>
(**some** _predicate_ _sequences+_) => result

Argument description:
- _predicate_ - a designator for a function of as many arguments as there are SEQUENCES.
- _sequence_  - a sequence.

PREDICATE is first applied to the elements with index '0' in each of
the SEQUENCES, and possibly then to the elements with index '1', and so
on, until a termination criterion is met or the end of the shortest of
the SEQUENCES is reached.

EVERY returns NIL as soon as any invocation of PREDICATE returns
false. If the end of a SEQUENCE is reached, EVERY returns T.
Thus, EVERY returns true if and only if every invocation of PREDICATE
returns true.

SOME returns the first non-nil value which is returned by an
invocation of PREDICATE. **If the end of a SEQUENCE is reached without
any invocation of the PREDICATE returning true, SOME returns false.**
Thus, SOME returns T if and only if some invocation of PREDICATE
returns true.

``` cl
(every #'characterp "abc") ;=> T
(some #'= '(1 2 3 4 5) '(5 4 3 2 1)) ;=> T (because of 3)
```

### mapc

(**mapc** _fn_ _lists+_) => the first list from lists argument

Argument description:
- _fn_   - function that takes as many arguments as there are lists
- _lists_ - lists which elements are processed in **parallel**

MAPC applies function FN to elements of lists with same index. Each application
result forgotten. Elemnts are processed only up to length of the shortest list
argument.

See MAPCAR, MAPCAN, MAPCON, DOLIST.

``` cl
(setq y '(1 2 3)) ;=> NIL
(mapc #'print y) ;=> prints 1 2 3 (perform operation on each element)
```

### count

(**count** _item_ _sequence_ :test :from-end :start :end :key :test :test-not) => integer

Argument description:
- _item_     - an item to be found
- _sequence_ - a sequence to be searched
- _test_     - function key and item comparison
- _from-end_ - direction of search, **default is NIL - forward**
- _start_    - starting position for search, **default is 0**
- _end_      - final position for search, **default is NIL - end of sequence**
- _key_      - function for extracting value **before** test
- _test_     - function for comparison of two values
- _test-not_ - function for comparison of two values

COUNT function counts specified elements in sequence. Return value is number of
occurrences or NIL if no occurance is not found.

See also COUNT-IF, POSITION, POSITION-IF, FIND, FIND-IF and MEMBER.

``` cl
(count #\s "Some sequence") ;=> 1
(count #\s "Some sequence" :key #'char-downcase) ;=> 2
(count #\s "Some sequence" :key #'char-downcase :start 1) ;=> 1
(count #\x "Some sequence") ;=> 0
(count '(1 2) #(9 3 (1 2) 6 7 8)) ;=> 0
(count '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) ;=> 1
(count 1 #(0 1 0 0 0 1 0) :from-end t) ;=> 2
```

### delete

(**delete** _item_ _sequence_ :from-end :test :test-not :start :end :count :key) => result-sequence

Omit matching elements. DELETE, DELETE-IF, and DELETE-IF-NOT are like REMOVE,
REMOVE-IF, and REMOVE-IF-NOT respectively, but they may **modify** SEQUENCE.

``` cl
(setq tester (list 1 2 4 1 3 4 5)) ;=> (1 2 4 1 3 4 5)
(delete 4 tester) ;=> (1 2 1 3 5)

(setq tester (list 1 2 4 1 3 4 5)) ;=> (1 2 4 1 3 4 5)
(delete 4 tester :count 1) ;=> (1 2 1 3 4 5)

(setq tester (list 1 2 4 1 3 4 5)) ;=> (1 2 4 1 3 4 5)
(delete 3 tester :test #'>) ;=> (4 3 4 5)

(setq tester (list 1 2 4 1 3 4 5)) ;=> (1 2 4 1 3 4 5)
(delete-if #'oddp tester) ;=> (2 4 4)
```

### find

(**find** _item_ _sequence_ :test :from-end :start :end :key) => element

Argument description:
- _item_     - an item to be found
- _sequence_ - a sequence to be searched
- _test_     - function key and item comparison
- _from-end_ - direction of search, *default is NIL - forward*
- _start_    - starting position for search, *default is 0*
- _end_      - final position for search, *default is NIL - end of sequence*
- _key_      - function for extracting value **before** test

FIND function searches for an element (item) satisfying the test.
Return value is element itself or NIL if item is not found.

See also POSITION, POSITION-IF, FIND, FIND-IF, MEMBER and FIND-ALL(from the book).

``` cl
(find #\s "Some sequence") ;=> #\s
(find #\s "Some sequence" :key #'char-downcase) ;=> #\S
(find #\s "Some sequence" :key #'char-downcase :start 1) ;=> #\s
(find #\x "Some sequence") ;=> NIL
(find '(1 2) #(9 3 (1 2) 6 7 8)) ;=> NIL
(find '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) ;=> (1 2)
(find 1 #(0 1 0 0 0 1 0) :from-end t) ;=> 1
```

### position

(**position** _item_ _sequence_ :test :from-end :start :end :key) => index or NIL

Argument description:
- _item_     - an item to be found
- _sequence_ - a sequence to be searched
- _test_     - function key and item comparison
- _from-end_ - direction of search, *default is NIL - forward*
- _start_    - starting position for search, *default is 0*
- _end_      - final position for search, *default is NIL - end of sequence*
- _key_      - function for extracting value **before** test

POSITION function searches for an element (item) satisfying the test. Return
value is index of such item or NIL if item is not found. Index is **relative** to
start of the sequence regardless of arguments.

See also POSITION-IF, FIND, FIND-IF and MEMBER.

``` cl
(position #\s "Some sequence") ;=> 5
(position #\s "Some sequence" :key #'char-downcase) ;=> 0
(position #\s "Some sequence" :key #'char-downcase :start 1) ;=> 5
(position #\x "Some sequence") ;=> NIL
(position '(1 2) #(9 3 (1 2) 6 7 8)) ;=> NIL
(position '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) ;=> 2
(position 1 #(0 1 0 0 0 1 0) :from-end t) ;=> 5
```

### reduce

(**reduce** _fn_ _sequence_ :initial-value :key :from-end :start :end) => an object

Argument description:
- _fn_            - a two argument function
- _sequence_      - a sequence
- _initial-value_ - an object
- _key_           - function for extracting values from sequence
- _from-end_      - direction flag, **default is NIL**
- _start_         - bounding index
- _end_           - bounding index

REDUCE (aka accumulate) applies function fn to its previous result and next element.
The result is what fn returned in last call. For the first call fn is called with either
initial-value and first element or first two elements.

See also MAPCAR, MAPCAN, MAP.

``` cl
(reduce #'list '(1 2 3 4)) ;=> (((1 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0) ;=> ((((0 1) 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0 :from-end t) ;=> (1 (2 (3 (4 0))))
(reduce #'list '(1 2 3 4) :from-end t) ;=> (1 (2 (3 4)))
(reduce (lambda (x y) (+ (* x 10) y)) '(1 2 3 4)) ;=> 1234
(reduce #'+ '(1 2 3 4)) ;=> 10
(reduce #'* '(1 2 3 4) :initial-value 1) ;=> 24
```

### substitute

(**substitute** _newitem_ _olditem_ _sequence_ :from-end :test :test-not :start :end :count :key) => sequence

Argument description:
- _newitem_   - an item to be placed
- _olditem_   - an item to be replaced
- _sequence_  - a sequence to be searched
- _from-end_  - direction of search, **default is NIL - forward**
- _test_      - function key and item comparison
- _start_     - starting position for search, **default is 0**
- _end_       - final position for search, **default is NIL - end of sequence**
- _count_     - limits the number of elements altered
- _key_       - function for extracting value **before** test

SUBSTITUTE, SUBSTITUTE-IF, and SUBSTITUTE-IF-NOT return a **copy** of
SEQUENCE in which each element that satisfies the test has been replaced
with NEWITEM.

### nth

(**nth** _n_ _list_) => an object

Argument description:
- _n_    - a non-negative integer
- _list_ - a list, which might be a dotted list or a circular list.

NTH locates the _Nth_ element of LIST, where the car of the LIST is the
"zeroth" element.

NTH may be used to specify a PLACE to SETF. Specifically,<br/>
```(setf (nth N LIST) NEW-OBJECT) <=> (setf (car (nthcdr N LIST)) NEW-OBJECT)```<br/>

See also ELT, FIRST, NTHCDR

``` cl
(nth 0 nil) ;=> NIL
(nth 0 '(foo bar baz)) ;=> FOO
(nth 3 '(foo bar baz)) ;=> NIL
```

### reverse

(**reverse** _sequence_) => a sequence

Argument description:
- _sequence_ - a sequence

REVERSE function makes new sequence with reverted order of elements. Elements in the
sub-list are not reversed.

See also MAP, MAPCAR and MAPCAN.

``` cl
(reverse '(1 2 3 4)) ;=> (4 3 2 1)
(reverse '(a (b c) d)) ;=> (D (B C) A)
(reverse '#(1 2 3 4)) ;=> #(4 3 2 1)
(reverse "hola") ;=> "aloh"
(reverse nil) ;=> NIL
```

### subseq

(**subseq** _sequence_ _start_ _end?_) => sequence

Argument description:
- _sequence_ - a sequence
- _start_    - bounding index
- _end_      - bounding index, **default NIL**

SUBSEQ function makes new sequence as a subseqence of argument.
Default ending index is end of sequence. SUBSEQ may be used with SETF.

See also COPY-SEQ and MAP.

``` cl
(subseq "hello world" 3) ;=> "lo world"
(subseq "hello world" 3 5) ;=> "lo"
(let ((a "hello world")) (setf (subseq a 3 5) "LO") a) ;=> "helLO world"
(let ((a "hello world")) (setf (subseq a 3 5) "YYY") a) ;=> "helYY world"
```

### aref

(**aref** _array_ _index+_) => element pointed by the _index_

Argument description:
- _array_ - an array
- _index_ - a list of valid array indices

AREF function accesses specified elements of arrays. Every array index is
counted from zero. Accessing out-of-bounds indices signals condition, or causes
crash and/or undefined behavior, depending on compilation safety mode. **Note that
vectors (including strings which are special vectors) are treated as one
dimensional arrays so AREF works on them too.**

AREF with conjunction of SETF may be used to set array elements.

See also: SVREF

``` cl
(aref "hola" 0) ;=> #\h
(aref "hola" 3) ;=> #\a
(aref #(5 3 6 8) 1) ;=> 3

;; Two dimensional
(aref (make-array '(10 10) :initial-element 'moo) 9 9) ;=> MOO

(let ((a (make-array '(3 3) :initial-element 'moo)))
      (setf (aref a 1 1) 'x) a) ;=> #2A((MOO MOO MOO) (MOO X MOO) (MOO MOO MOO))
````

### char

(**char** _string_ _index_) => character

Argument description:
- _string_ - a string
- _index_  - a valid array index for the string

CHAR access the element of string specified by index. Could be used in SETF:
```(setf (char string index) new-character)```

``` cl
(setq my-simple-string (make-string 6 :initial-element #\A)) ;=>  "AAAAAA"
(schar my-simple-string 4) ;=>  #\A
```

### bit

(**bit** _bit_ _subscripts?_) => bit

Argument description:
- _bit_        - for bit, a bit array
- _subscripts_ - a list of valid array indices for the bit-array.

BIT and SBIT access the bit-array element specified by subscripts.
BIT is exactly like AREF but requires array of bits. The result is 0 or 1.

### concatenate

(**concatenate** _result-type_ _sequence+_) => sequence

Argument description:
- _result-type_ - sequence type specifier or NIL
- _sequence_    - sequences

CONCATENATE creates new sequence and fills it with data from arguments.

See also MAPCAN.

``` cl
(concatenate 'string "hello" " " "world") ;=> "hello world"
(concatenate 'vector "hello" " " "world") ;=> #(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d)
(concatenate 'vector '(1 2) '(3 4)) ;=> #(1 2 3 4)
```

### eq
### equal
### equalp

(**eq** _object1_ _object2_) => T or NIL<br/>
(**equal** _object1_ _object2_) => T or NIL<br/>
(**equalp** _object1_ _object2_) => T or NIL

Argument description:
- _object1_	- first object
- _object2_	- second object

EQ function compares object identity. It works for symbols and identical
objects. It is not suitable for comparing numbers - see **EQL** and **=**. Result is
true if they are same, otherwise false.

``` cl
(eq 'moo 'moo) ;=> T
(eq 1 2) ;=> NIL
(eq 1234567890123456789 1234567890123456789) ;=> NIL
(eq (cons 1 2) (cons 1 2)) ;=> NIL
(let ((x (cons 1 2))) (eq x x)) ;=> T
```

EQL function compares object identity, numbers and characters. Numbers are
considered as equal only when they have the both same value and type. Result is
true if they are same, otherwise false.

``` cl
(eql 'moo 'foo) ;=> NIL
(eql 1 1) ;=> T
(eql 1 2) ;=> NIL
(eql 1234567890123456789 1234567890123456789) ;=> T
(eql 1.0 1) ;=> NIL
(eql 1.0 1.0) ;=> T
(eql (cons 1 2) (cons 1 2)) ;=> NIL
(let ((x (cons 1 2))) (eql x x)) ;=> T
```

EQUAL function compares same things as eql, additionally result is true under
some other situations: conses are compared recursively (in both car and cdr
part), string and bit-vectors are compared element-wise. Result is true if they
are same, otherwise false.

``` cl
(equal #*1010101 #*1010101) ;=> T
(equal (vector 2 3 4) (vector 2 3 4)) ;=> NIL
(equal (cons 1 2) (cons 1 2)) ;=> T
(let ((x (cons 1 2))) (equal x x)) ;=> T
(equal 'moo 'moo) ;=> T
(equal 1.0 1) ;=> NIL
(equal 1.0 1.0) ;=> T
```

EQUALP function compares same things as equal, additionally result is true under
some other situations: conses are compared recursively (in both car and cdr
part), any sequence is compared recursively (element-wise), strings and
characters are compared case insensitively. Result is true if they are same,
otherwise false.

``` cl
(equalp "moo" "MoO") ;=> T
(equalp "moo" "moo ") ;=> NIL
(equalp (cons 1 2) (cons 1 2)) ;=> T
(let ((x (cons 1 2))) (equalp x x)) ;=> T
(equalp 'moo 'moo) ;=> T
(equalp 'moo 'foo) ;=> NIL
(equalp "a" 'a) ;=> NIL
(equalp 1 2) ;=> NIL
(equalp 1.0 1) ;=> T
(equalp 1.0 1.0) ;=> T
```

All four begin with the letters _eq_, with more letters meaning the predicate
considers more objects to be equal. The simplest predicate is **eq**, which tests
for the exact same object. Next, **eql** tests for objects that are either **eq** or are
equivalent numbers, **equal** tests for objects that are either **eql** or are lists or
strings with **eql** elements. Finally, **equalp** is like equal except it also matches
upper- and lowercase characters and numbers of different types.

### copy-tree
### tree-equal

(**copy-tree** _tree_) => new-tree<br/>
(**tree-equal** _tree1_ _tree2_ :test) => T or NIL

Argument description:
- _tree_ - a tree represented like list e.g. ```((a b) ((c)) (d e))```

COPY-TREE treats passed list as a tree and creates a copy of a tree of conses.
TREE-EQUAL is similar to EQUAL but is more powerful because allows :test keyword.

``` cl
(setf tree '((a b) ((c)) (d e)))
(tree-equal tree (copy-tree tree)) ;=> T
```
### subst
### sublis

(**subst** _new_ _old_ _tree_ :key :test :test-not)<br/>
(**sublis** _(old . new)-alist_ _tree_ :key :test :test-not)

This two functions substitute a new expression for an old one anywhere within
the tree. **subst** substitutes a single value for another, while **sublis**
takes a list of substitutions in the form of an association list of (old . new)
pairs.

**Note** that _the order_ of old and new in the alist for **sublis** is reversed from
the order of arguments to **subst**.

``` cl
(subst 'new 'old '(old ((very old))) ;=> (NEW ((VERY NEW)))
(sublis '((old . new)) '(old ((very old)))) ;=> (NEW ((VERY NEW)))
```
### rem
### mod

(**rem** _number_ _divisor_) => remainder<br/>
(**mod** _number_ _divisor_) => modulus

REM performs the operation 'truncate' on _number_ and _divisor_ and
returns the remainder of the 'truncate' operation.

MOD performs the operation 'floor' on _number_ and _divisor_ and returns
the remainder of the 'floor' operation.

``` cl
(rem -1 5) ;=> -1
(mod -1 5) ;=> 4
(mod 13 4) ;=> 1
(rem 13 4) ;=> 1
(mod -13 4) ;=> 3
(rem -13 4) ;=> -1
```

### intersection
### nintersection

(**intersection** _list1_ _list2_ :key :test :test-not) => list<br/>
(**nintersection** _list1_ _list2_ :key :test :test-not) => list

INTERSECTION and NINTERSECTION return a list that contains every
element that occurs in both _list1_ and _list2_.

NINTERSECTION is the **destructive** version of INTERSECTION.  It
performs the same operation, but may destroy _list1_ using its cells to
construct the result. _list2_ is not destroyed.

``` cl
(setq list1 (list 1 1 2 3 4 a b c "A" "B" "C" "d")
      list2 (list 1 4 5 b c d "a" "B" "c" "D"))
 ;=> (1 4 5 B C D "a" "B" "c" "D") (more than one assignment at once)

(intersection list1 list2) ;=> (C B 4 1 1)
(intersection list1 list2 :test 'equal) ;=> ("B" C B 4 1 1)
(intersection list1 list2 :test #'equalp) ;=> ("d" "C" "B" "A" C B 4 1 1)

(nintersection list1 list2) ;=> (1 1 4 B C)
```

### union
### nunion

(**union** _list1_ _list2_ :test :test-not :key) => list<br/>
(**nunion** _list1_ _list2_ :test :test-not :key) => list

Argument description:
- _list1_    - list to be joined
- _list2_    - other list to be joined
- _key_      - function for extracting value before test
- _test_     - function for comparison of two values
- _test-not_ - function for comparison of two values

Combine _list1_ and _list2_ using a set-union operation.
The resulting list contains all items that appear in either _list1_ or _list2_.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original _list1_ and _list2_.

``` cl
(union '(1 2 3) '(2 3 4)) ;=> (1 2 3 4)
(union '((1) (2) (3)) '((2) (3) (4))) ;=> ((3) (2) (1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :test #'equal) ;=> ((1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :key #'first) ;=> ((1) (2) (3) (4))
```

### set-difference
### nset-difference

(**set-difference** _list1_ _list2_ :test :test-not :key)<br/>
(**nset-difference** _list1_ _list2_ :test :test-not :key)

Argument description:
- _list1_ - a list
- _list2_ - a list
- _key_   - function for extracting value before test
- _test_  - function key and item comparison

SET-DIFFERENCE function computes set difference, that is a list of elements that appear in
_list1_ but do not appear in _list2_. Test argument specifies comparison operator. **Default
comparison operator is EQL**. _Key_ argument specifies function for extracting relevant
value from list items. Resulting item order is not specified.

See also SET-EXCLUSIVE-OR, UNION and INTERSECTION.

``` cl
(set-difference '(a b c) '(b c d)) ;=> (A)
(set-difference '("a" "b" "c") '("b" "c" "d")) ;=> ("c" "b" "a")
(set-difference '("a" "b" "c") '("b" "c" "d") :test #'equal) ;=> ("a")
(set-difference '((a . 2) (b . 3) (c . 1))
                '((b . 1) (c . 2) (d . 4)) :key #'car) ;=> ((A . 2))
```

### subsetp

(**subsetp** _list1_ _list2_ :test :test-not :key)<br/>
(**nsubsetp** _list1_ _list2_ :test :test-not :key)

Return true if _list1_ is a subset of _list2_.
I.e., if every element of _list1_ also appears in _list2_.

Set operations with different types:

| lists           | integers   | bit vectors |
|:----------------|------------|-------------|
| intersection    | logand     | bit-and     |
| union           | logior     | bit-ior     |
| set-difference  | logandc2   | bit-andc2   |
| member          | logbitp    | bit         |
| length          | logcount   |             |

``` cl
(intersection '(a bed) '(a b e)) ;=> (A B)
(bit-and #*11110 #*11001) ;=> #*11000
(logand #bllll0 #bll001) ;=> 24 = #bll000
```

### check-type

(**check-type** _form_ _type_ _string?_)  => no error or continuable error

Verify that _form_ is of type _type_; **signal an continuable error if not.**
_string_ is an optional description of the desired type.

``` cl
(defun my-sqrt (x)
   (check-type x number)
   (* x x))
```

### print
### print1
### printc

(**print** _object_ _output-stream?_) => object<br/>
(**print1** _object_ _output-stream?_) => object<br/>
(**printc** _object_ _output-stream?_) => object

The function `print` prints any object on a new line, with a space following it. `prin1`
will print any object without the new line and space. For both functions, the object is
printed in a form that could be processed by `read`. The function `princ` is used to print
in a human-readable format. This means that `read` cannot recover the original form; read
would interpret it as two symbols, not one string.

|  Shortcut      |          Equivalent to                              |
|----------------|-----------------------------------------------------|
| (PRINC <obj>)  |  (WRITE <obj> :ESCAPE NIL :READABLY NIL)            |
| (PRIN1 <obj>)  |  (WRITE <obj> :ESCAPE T)                            |
| (PRINT <obj>)  |  (PROGN (TERPRI) (PRIN1 <obj>) (WRITE-CHAR #\Space))|
| (PPRINT <obj>) |  (PROGN (TERPRI) (WRITE <obj> :ESCAPE T :PRETTY T)) |

As a rule of thumb, PRINC should be used to generate output for humans, whereas
PRIN1’s output is intended for the Lisp reader. PRINT and PPRINT are variants of PRIN1;
they only differ in how whitespace is arranged between objects.

### read

(**read** _input-stream?_ _eof-error-p?_ _eof-value?_ _recursive-p_) => an object

Argument description:

- _input-stream_ - an input stream, **default is standard input**
- _eof-error-p_  - a boolean, true (default) is EOF should be signaled
- _eof-value_    - an object that is returned as EOF value
- _recursive-p_  - flag to note recursive processing

READ function reads arbitrary readable Lisp object(reads Lisp code) from input stream.
Reading process uses \*read-table\*.

Note that \*read-eval\* global variable controls read-time evaluation.

See also: READ-LINE, READ-FROM-STRING, WRITE and WRITE-LINE

``` cl
(let ((s (make-string-input-stream "(1 2 3)"))) (read s)) ;=> (1 2 3)
(let ((s (make-string-input-stream "#(1 2 3)"))) (read s)) ;=> #(1 2 3)
(let ((s (make-string-input-stream "\"hola\""))) (read s)) ;=> "hola"
```

### cerror

(**cerror** _continue-format-control_ _error-format-string_ _arguments?_) => NIL

CERROR has two required arguments. The first argument is a format control string
that you'll use to tell the program's user what will happen upon continuing
from the error. The second argument is a condition designator
(a format control string, a symbol that names a condition, or a condition
object) used to tell the program's user about the error.

If _error-format-string_ is a condition (see DEFINE-CONDITION), _arguments_
can be supplied.

``` cl
(defun real-sqrt (n)
  (when (minusp n)
    (setq n (- n))
    (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
  (sqrt n))
```

### nconc

(**nconc** &rest list\*) => changed list

Like APPEND, NCONC returns a concatenation of its list arguments, but it builds its result
in the following way: for each nonempty list it's passed, NCONC sets the CDR of the list's
last cons cell to point to the first cons cell of the next nonempty list. It then returns
the first list, which is now the head of the spliced-together result. Thus:

```cl
(defparameter *x* (list 1 2 3))
(nconc *x* (list 4 5 6)) ;=> (1 2 3 4 5 6)
*x* ;=> (1 2 3 4 5 6)
```

## Chapter-4

### push

(**push** _item_ _place_) => list

Argument description:
- _item_  - an object
- _place_ - a place which can contain any object, but usually list

PUSH macro **modifies** variable or generally place. It makes a new cons cell filled
with item as car and previous value as cdr, that is effectively prepends new
item to list found at the place.

See also PUSH-NEW, ACONS and POP.

``` cl
(let ((x 'x)) (push 4 x) x) ;=> (4 . X)
(let ((x '(3 2 1))) (push 4 x) x) ;=> (4 3 2 1)
(let ((x '((a b c) (3 2 1) (e f g)))) (push 4 (second x)) x) ;=> ((A B C) (4 3 2 1) (E F G))
```

### fresh-line

(**fresh-line** &optional stream) => T or NIL

FRESH-LINE writes a newline unless it can determine that the output stream is already at
the beginning of a new line; FRESH-LINE returns T if it actually wrote a newline, and NIL
otherwise.

### mapcan

(**mapcan** _fn_ _lists+_) => list

Argument description:
- _fn_     - function that takes as many arguments as there are lists
- _lists+_ - lists(one or more) which elements are processed in parallel

MAPCAN applies function FN to elements of lists with same index. Each application result
is **concatenated** into resulting list.

MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the way they build up their
result. While MAPCAR and MAPLIST build a completely new list to hold the results of the
function calls, MAPCAN and MAPCON build their result by splicing together the results -
which must be lists - as if by NCONC.

MAPCAN is used with a function to return a variable number of items to be included in an
output list. When the function returns zero or one items, the function serves as a
filter. For example,
``` cl
(mapcan #'(lambda (x) (when (and (numberp x) (evenp x)) (list x)))
        '(1 2 3 4 x 5 y 6 z 7))
```

See MAPCAR.

``` cl
(mapcan (lambda (x) (list (+ x 10) 'x)) '(1 2 3 4)) ;=> (11 X 12 X 13 X 14 X)
(mapcan #'list '(a b c d)) ;=> (A B C D)
(mapcan #'(lambda (x) (if (= x 10)
                          nil
                          (list x))) list) ;=> the same as (remove 10 list)
(mapcan (lambda (x) (if (> x 0) (list x) nil)) '(-4 6 -23 1 0 12 )) ;=> (6 1 12)
```

Let's summaries:

| Given: ((A) (B C) (D)) | ignore results | collect each result | merge results |
|:-----------------------|----------------|---------------------|---------------|
| Work on each element   | _mapc_         | _mapcar_            | _mapcan_      |
| apply ```reverce```:   |                | ((A) (C B) (D))     | (A C B D)     |
| Work on each tail      | _mapl_         | _maplist_           | _mapcon_      |
| apply ```reverce```:   |                | ( ((D) (B C) (A)) ((D) (B C)) ((D)) )  | ((D) (B C) (A) (D) (B C) (D)) |


### consp

(**consp** _expression_) => T or NIL

CONSP function returns true if the argument refers to `cons` cell, otherwise it returns
false. If CONSP gets the empty list NIL as its argument, it returns a value of NIL
because the empty list, in contrast to non-empty lists, is not built by multiple calls to
CONS. NIL is, however, an atom.

The following calls demonstrate this:
```cl
(consp NIL) ;=> NIL
(atom NIL)  ;=> T

;; But:
(consp '(NIL)) ;=> T
```

Here the list (NIL) is created by the "CONSing" of NIL to NIL:
```cl
(cons NIL NIL) ;=> (NIL)

;; T is just an atomic expression:
(consp t) ;=>NIL
```

``` cl
(consp nil) ;=> NIL
(consp "moo") ;=> NIL
(consp (cons 1 2)) ;=> T
(consp '(1 . 2)) ;=> T
(consp '(1 2 3 4)) ;=> T
(consp (list 1 2 3 4)) ;=> T
```

### listp

(**listp** _expression_) => T or NIL

In contrast to CONSP, LISTP also accepts the empty list (NIL) as a list.

```cl
(listp '(a b c)) ;=> T
(listp NIL) ;=> T
(listp 12)  ;=> NIL
(listp t)   ;=> NIL
```

### copy-list

(**copy-list** _list_) => copy of the list

This returns a list that is equal to list, but not eq. **Only the top level of list
structure is copied**; that is, copy-list copies in the cdr direction but not in the car
direction.

See also: COPY-SEQ and COPY-TREE.

### read-from-string

(**read** _string_ _eof-error-p?_ _eof-value?_ &key _start_ _end_ _preserve-whitespace_)
=> an object, position

READ-FROM-STRING works just like READ, but lets us read a syntax expression (or any other
basic Lisp datatype) from a string instead of directly from the console.

## Chapter-6

### handler-case

A handler serves the simple purpose of tying a condition to a restart. This means that if
that condition is raised, that restart will automatically be chosen, meaning that we don't
get dumped into the debugger.

Many condition handlers simply want to unwind the stack to the place where they were
established and then run some code. The macro HANDLER-CASE establishes this kind
of condition handler. The basic form of a HANDLER-CASE is as follows:
``` cl
(handler-case  expression
   error-clause*)
```
where each error-clause is of the following form: `(condition-type ([var]) code)`.

If the expression returns normally, then its value is returned by the HANDLER-CASE. The
body of a HANDLER-CASE must be a single expression; you can use PROGN to combine several
expressions into a single form. If, however, the expression signals a condition that's an
instance of any of the condition-types specified in any error-clause, then the code in the
appropriate error clause is executed and **its value returned** by the HANDLER-CASE. The
`var`, if included, is the name of the variable that will hold the condition object when
the handler code is executed. If the code doesn't need to access the condition object, you
can omit the variable name.

See also: HANDLER-BIND

### progv

PROGV creates new dynamic bindings for variables whose names are determined at runtime.
This is mostly useful for implementing embedded interpreters for languages with
dynamically scoped variables. The basic skeleton is as follows:

``` cl
(progv symbols-list values-list
       body-form*)
```
where _symbols-list_ is a form that evaluates to a list of symbols and _values-list_ is a
form that evaluates to a list of values. Each symbol is dynamically bound to the
corresponding value, and then the _body-forms_ are evaluated. The difference between PROGV
and LET is that because _symbols-list_ is evaluated at runtime, the names of the variables
to bind can be determined dynamically.

STYLE:<br/>
If you want to bind a list of values to a list of lexical variables, use<br/>
```(MULTIPLE-VALUE-BIND (..) (VALUES-LIST ..) ..)```<br/>
or<br/>
```(MULTIPLE-VALUE-SETQ (..) (VALUES-LIST ..))```<br/> instead.

### adjoin

(**adjoin** _item_ _list_ :test :test-not :key) => new list

Return _item_ consed onto the front of _list_ only if it's not already there.
**Otherwise, return _list_ unmodified.**
```
(adjoin item list) == (if (member item list)
                          list
                          (cons item list))
```
See also: PUSHNEW

### merge

(**merge** _result-type_ _sequence+_ _predicate_ :key) => sequence

The MERGE function takes two sequences and a predicate and returns a sequence produced by
merging the two sequences, according to the predicate. It's related to the two sorting
functions in that if each sequence is _already sorted by the same predicate_, then the
sequence returned by MERGE will also be sorted. _result-type_ in MERGE must be a type
descriptor specifying the type of sequence to produce.
```cl
;; Note that sequence are already sorted

(merge 'vector #(1 3 5) #(2 4 6) #'<) ;=> #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<)   ;=> (1 2 3 4 5 6)
```
Like the sorting functions, MERGE takes a **:key** argument. A useful example of a **:key**
function would be a component selector function for a DEFSTRUCT structure, used to merge a
sequence of structures. In other words selects DEFSTRUCT field that is used for merging.
```cl
;; p. 210
(defun insert-path (path paths)
  "Put PATH into the right position, sorted by total cost."
  (merge 'list (list path) paths #'< :key #'path-total-cost))
```

TIP:
```cl
Here's a quick way to add an item to a sorted list:

(merge 'list (list item) sorted-list test)
```

## Chapter-9

### sharpsign

It is a conditional read macro characters ```#+``` for conditional hiding or ```#-``` for
revealing of code.  For example, ```#'fn``` is read as ```(function fn)```. The character
sequence ```#+``` is defined so that _feature_ expression reads as expression if the
_feature_ is defined in the current implementation, and as nothing at all if it is
not. The sequence ```#-``` acts in just the opposite way. Here is the details:

- Every COMMON L ISP has a global special variable called ***FEATURES***, which
  holds a list of symbols. These are typically keyword symbols, but they don’t
  have to be.

- Every Lisp’s list of features will be different (that being the whole point), but a
  couple of things can be expected:

  • One or more symbols identifying the vendor, like :LISPWORKS, :SBCL, or :CLISP.
  • Sometimes also symbols identifying the version, like :LISPWORKS6 or even :LISPWORKS6.1.
  • Symbols identifying the underlying operating system or architecture, like :LINUX or :X86.
  • Symbols denoting capabilities the Lisp might or might not have (may be depending on
    how it was built or on the platform it runs on), like :SB-THREAD for versions of
    SBCL with multi-threading.

- User code and library code can modify this list and might add symbols to it.
  For example, once you’ve loaded the CFFI library, it has added :CFFI to *FEATURES*.

- If your Lisp reads something like
```cl
#+:foo (bar)
```
then it’ll first read the symbol :FOO an then it’ll first read the symbol :FOO and
check whether :FOO is in *FEATURES*. If it is, it will simply continue reading the (BAR)
form. If it is not, it will skip that form.

- Symbols will be read as if the keyword package were the current package, so
the above could also have been written like so:
```cl
#+foo (bar)
```
But the following would be different because it’d mean to look for the symbol
CL-USER::FOO in *FEATURES*:
```cl
#+cl-user::foo (bar)
```
- If the form following the ```#+``` is to be ignored, it will be read in a special mode
  so that the reader will find the end of the form without creating symbols, complaining
  about non-existent packages, and so on. So, if you’re on SBCL, then this

```cl
#+:lispworks (fli:register-module :thing-fish)
```
won’t result in error messages, even if there is no package named "FLI" in your image.

- Instead of a symbol, you can also have expressions behind #+ that are comprised of
  symbols  and the logical connectives AND, OR, and NOT, with the usual meaning. For
  example, this

```cl
#+(or cmucl (and sbcl (not os-windows))) (do-it)
```
would mean to skip the (DO-IT) form unless we are on CMUCL or on SBCL, but in the latter
case, only if the OS isn’t Microsoft Windows.

- There’s also ```#-```, which is simply the opposite of #+. (So, if it weren’t there, you
  could simulate it with NOT.)

As almost all of the features aren’t standardized; you can’t really rely on certain
things. For example, what do you need to be sure you’re on Mac OS X? Will :DARWIN
be in *FEATURES* or :MACOS or something else?

How does ```#+(OR)``` work? The (one) form following #+(OR) will be ignored. It is just a
_clever trick_ relying on the fact that, in order to be logically consistent, the
expression (OR) must always evaluate to NIL (try it), so this is as if you had written
```#+FOO``` with :FOO not being in your list of features.  (Except that there’s no symbol of
which you can be absolutely sure that it’s not in somebody’s *FEATURES* somewhere.)

### multiple-value-bind

One great feature in Common Lisp is the ability for a single form to return _multiple
values_. The key thing to understand about multiple values is that returning multiple
values is quite different from returning a list - if a form returns multiple values,
unless you do something specific to capture the multiple values, all but the _primary
value_ will be silently discarded. There are two aspects to using multiple values -
returning multiple values and getting at the non primary values returned by forms that
return multiple values. **MULTIPLE-VALUE-BIND** creates variable bindings like LET does,
filling them with the multiple values returned by a form.

```cl
(defun show-value (key hash-table)
  ;; Specify a list of the variables you want to bind each value to, followed by the
  ;; expression that will return multiple values.
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
      (format nil "Value ~a actually present." value)
      (format nil "Value ~a because key not found." value))))
```

See also: VALUES, VALUES-LIST

### prog1

### multiple-value-prog1

In particular, the PROG1 macro, which evaluates a number of forms like a PROGN before
returning the value of the first form, returns that form's _primary value_ only. Likewise,
PROG2, which returns the _value of the second_ of its subforms, returns only the primary
value. _The special operator MULTIPLE-VALUE-PROG1 is a variant of PROG1 that returns all
the values returned by the first form_. It's a minor wart that PROG1 doesn't already behave
like MULTIPLE-VALUE-PROG1, but neither is used often enough that it matters much. **The OR
and COND macros are also not always transparent to multiple values, returning only the
primary value of certain subforms.**

### pop

### time

### proclaim

PROCLAIM names a function for making global declarations.

```cl
(defvar *x*)

;; is almost the same as

(proclaim '(special *x*))
```

DECLAIM names a macro for making global declarations (like PROCLAIM) which are also
effective at _compile-time_.

```cl
(proclaim '(special *x*))
(defun foo () (print *x*))
```

the compiler will complain that foo reads an unknown special variable ****x****, while

```cl
(declaim (special *x*))
(defun foo () (print *x*))
```
will cause no warnings.

DECLARE is just a symbol for making local declarations in the beginning of some
forms. DECLARE is the easy one - it has a block limited scope. Here is an example:

Both PROCLAIM and DECLARE give the system some supplemental information in the form of a
declaration. For example, you an use a declaration for improving the efficiency of
executing a program without affecting the result of the execution. By inserting a DECLARE
expression in the definition of factorial:

```cl
(defun fact(x)
  (declare (integer x))
  (if (zerop x) 1
      (* x (fact (1- x)))))
```
you give the system the information that the parameter is always an integer.

### unwind-protect

You'll occasionally use UNWIND-PROTECT directly. More often you'll use it as the basis for
**WITH-** style macros, that evaluate any number of body forms in a context where they
have access to some resource that needs to be cleaned up after they're done, regardless of
whether they return normally or bail via a restart or other nonlocal exit.

For example, if you were writing a database library that defined functions `open-connection`
and `close-connection`, you might write a macro like this:

```cl
(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
    (unwind-protect (progn ,@body)
      (close-connection ,var))))
```

and not have to worry about closing the database connection, since the UNWIND-PROTECT will
make sure it gets closed no matter what happens in the body of the
with-database-connection form.

### intern

(**intern** _character-string_ _package*_) => symbol, status

Argument description:
- _character-string_ - a string
- _package_          - a package designator, default is current package.

INTERN is multiple-valued function. The first value is the symbol that you wanted.
The second one is one of the following:
```
:intern     (if it has been registered as an internal symbol)
:external   (if it has been registered as an external symbol)
:inherited  (if it has been registered as an external symbol of the package used)
:nil        (if newly created)
```
Here is some examples:

```cl
(intern "NIL") ;=> nil :inherited

(intern "MOO") => MOO, NIL
(intern "MOO") => MOO, :INTERNAL
```

### the

(**the** _value-type_ _form_) => result*

Argument description:
- _value-type_ - specifies type
- _form_       - the form that will be evaluated. _Result should be the same as specified._

```cl
(let ((i 100))
    (declare (fixnum i))
    (the fixnum (1+ i))) ;=>  101
```

### svref

(**svref** _vector_ _index+_) => element pointed by the _index_

Argument description:
- _vector_ - a vector
- _index_  - a list of valid vector indices

The same as AREF but for vector.

See also: AREF

### simple-vector

SIMPLE-VECTOR and SIMPLE-ARRAY. Simple vectors and arrays are those that do not share
structure with other arrays, do not have fill pointers, and are not adjustable. In many
implementations it is faster to AREF a SIMPLE-VECTOR than a VECTOR. It is certainly much
faster than taking an ELT of a sequence of unknown type. Declare your arrays to be simple
(if they in fact are).

### simple-array

An array that is not displaced or adjustable.

The concept of a simple array exists to allow the implementation to use a specialized
representation and to allow the user to declare that certain values will always be simple
arrays.

In HyperSpec for SIMPLE-ARRAY definition is used word _dimension_ which is a bit
misleading when you read ```(simple-array fixnum *)```. It is equivalent to
```(simple-array(*))```. Actually the length of the list is the dimension, values are the
dimensions length. When it is a star ```*``` it means unknown length.

## Chapter-11

### get

(**get** _symbol_ _indicator_ _default*_) => value

_In Lisp, every symbol has a property list._ Property lists provide basically the same
facilities as association lists and hash tables: You can store a value in a property list
under a given key (called an indicator), and later look things up in the property list by
supplying the indicator. Property lists are organized as lists of alternating indicators
and values, like this: ```(ind-1 val-1 ind2-2 val-2 ...)```

_Note that there is no way to distinguish an absent property from one whose value is default._

The GET function retrieves a property of a symbol given the indicator. SETF understands
GET as a place description; that is how new properties are stored on the property
list. Let’s give the symbol FRED a property called SEX with value MALE, a property called
AGE with value 23, and a property called SIBLINGS with value (GEORGE WANDA).

```cl
(setf (get 'fred 'sex) 'male)
(setf (get 'fred 'age) 23)
(setf (get 'fred 'siblings) '(george wanda))

;; The actual property list of FRED looks like this: (siblings (george wanda) age 23 sex male)
(symbol-plist 'fred)

(get 'fred 'age) ;=> 23

;; The value of a property can be changed at any time. Suppose FRED has a birthday:
(incf (get 'fred 'age)) ;=> 24
(get 'fred 'age) ;=> 24
```

Like GETF, GET is SETFable, so you can attach arbitrary information to a symbol like this:<br/>
```(setf (get 'some-symbol 'my-key) "information")```

See also: GETF

### pushnew

(**pushnew** _item_ _place_) => list

PUSHNEW is a generalized assignment operator like PUSH, but it first checks to make sure
the element is not a member of the list, so it is useful for adding an element to a set.

### values

TODO

### catch

TODO

### terpri

TODO

## Koans

Interesting code taken from **Lisp-koans**

_(work in progress)_

## Misc

### getf

(**getf** _place_ _key_ _default?_) => list

Argument description:
- _place_   - property list
- _key_     - keying value, also know as indicator
- _default_ - answer when key-value pair is not found, default is NIL

GETF function searches supplied _plist_ for value with matching key. _plist_ is list of even
number of items. Each item pair specifies key and value. I.e. (K1 V1 K2 V2 ...). Return
value is either value for first matching key, or specified default. Keys are matched by **EQ**
function, therefore only suitable values are symbols and integers.

See also: FIND, ASSOC

```cl
(getf '(a b 4 d a x) 'a) ;=> B
(getf '(a b 4 d a x) 'x) ;=> NIL
(getf '(a b 4 d a x) 'x 'not-found) ;=> NOT-FOUND
(getf '(a b c) 'c) ;=> ERROR: malformed property list: (A B C)
```
