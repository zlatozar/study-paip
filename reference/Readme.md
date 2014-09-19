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
    - [assoc](#assoc)
    - [list](#list)
    - [make-list](#make-list)
- [Chapter-3](#chapter-3)
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
- [Chapter-4](#chapter-4)
    - [push](#push)
    - [fresh-line](#fresh-line)
- [Koans](#koans)

## Chapter-1

### append

(**append** _list\*_) => list

Argument description:
- _list_ -  lists to be concatenated

APPEND function concatenates list arguments into one list. Resulting list is
shallow copy of specified lists except for the last which is directly shared.

See also MAPCAN, CONS, LIST, LIST\*.

``` cl
(append) => NIL
(append '(1 2 3)) => (1 2 3)
(append '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
(append '(1 2 3) '(4 5 6) '(7 8 9)) => (1 2 3 4 5 6 7 8 9)
(let ((x '(tail list))) (eq x (cddr (append '(front list) x)))) => T
```

### length

(**length** _sequence_) => N

Argument description:
- _sequence_ - a proper sequence
- _N_        - a non-negative integer

Returns the number of elements in SEQUENCE.
If SEQUENCE is a vector with a fill pointer, the active length as specified by
the fill pointer is returned.

See also LIST-LENGTH

``` cl
(length nil) => 0
(length "abc") => 3
(setq str (make-array '(3) :element-type 'character
                           :initial-contents "abc"
                           :fill-pointer t)) => "abc"
(length str) => 3
(setf (fill-pointer str) 2) => 2
(length str) => 2
```

### mapcar

(**mapcar** _fn_ _list+_) => list

Argument description:
- _fn_    - function that takes as many arguments as there are lists
- _list+_ - lists which elements are processed in **parallel**

MAPCAR applies function FN to elements of lists with same index.
Each application result is put into resulting list. Length of resulting list
is the length of the shortest list argument.

See also MAPCAN and MAPPEND(from the book)

``` cl
(mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)
(mapcar #'round '(1.3 2.7 3.4 4.5)) => (1 3 3 4)
(mapcar #'list '(123 symbol "string" 345) '(1 2 3))
                         => ((123 1) (SYMBOL 2) ("string" 3))
(mapcar #'* '(3 4 5) '(4 5 6)) => (12 20 30)
```

### member

(**member** _item_ _list_ :test :key) => tail or NIL

Argument description:
- _item_   - an item to be found
- _list_   - a list to be searched
- _test_   - function key and item comparison
- _key_    - function for extracting value before test

MEMBER function searches a list for the first occurrence of an element (item)
satisfying the test. Return value is tail of the list starting from found
element or NIL when item is not found.

See also MEMBER-IF, POSITION, POSITION-IF, FIND, FIND-IF and FIND-ALL(from the book).

``` cl
(member 1 '(0 1 0 0 0 1 0)) => (1 0 0 0 1 0)
(member 2 '(0 1 0 0 0 1 0)) => NIL
(member #\h '(#\H #\o #\l #\a)) => NIL
(member #\h '(#\H #\o #\l #\a) :test #'char-equal) => (#\H #\o #\l #\a)
(member #\h '(#\H #\o #\l #\a) :key #'char-downcase) => (#\H #\o #\l #\a)
```

### apply

(**apply** _fn_ _args\*_) => value

Argument description:
- _fn_    - a function designator
- _args_  - call arguments

APPLY function call supplied function with specified arguments.
Argument list is constructed as
``` cl
(append (but last args) (first (last args))) ;; see second example
```
Note that there is limitation of maximal number of arguments,
see CALL-ARGUMENTS-LIMIT constant.

See also FUNCALL, LAMBDA and EVAL.

``` cl
(apply #'+ 1 2 3 '(4 5 6)) => 21
(apply 'cons '((+ 2 3) 4)) => ((+ 2 3) . 4)
```

### null

(**null** object) => T or NIL

Argument description:
- _object_ - an object

NULL function returns true if the argument is NIL, otherwise it returns
false. NULL is identical to NOT, but used in conjunction with list processing
unlike NOT which is used in boolean logic processing.

``` cl
(null nil) => T
(null t) => NIL
(null '()) => T

(null '(1 2 3)) => NIL
(null 234.4) => NIL
(null "lisp") => NIL
```

### numberp

(**numberp** _object_) => T or NIL

Argument description:
- _object_ - an object

Returns true if OBJECT is of type NUMBER; otherwise, returns false.

``` cl
(numberp 12) => T
(numberp nil) => NIL
(numberp (cons 1 2)) => NIL
```

### atom

(**atom** _object_) => T or NIL

Argument description:
- _object_ -  an object

ATOM function returns true if the argument is not a cons cell, otherwise it
returns false.

See CONS and LIST.

``` cl
(atom nil) => T
(atom 'some-symbol) => T
(atom 3) => T
(atom "moo") => T
(atom (cons 1 2)) => NIL
(atom '(1 . 2)) => NIL
(atom '(1 2 3 4)) => NIL
(atom (list 1 2 3 4)) => NIL
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
(elt "hola" 0) => #\h
(elt "hola" 3) => #\a
(elt #(5 3 6 8) 1) => 3
(elt '(5 3 6 8) 1) => 3
(let ((a (list 1 2 3 4))) (setf (elt a 1) 'x) a) => (1 X 3 4)
(let ((a (copy-seq "hola"))) (setf (elt a 1) #\O) a) => "hOla"
```

### assoc

(**assoc** _item_ _alist_ :key :test) => cons cell or NIL

Argument description:
- _item_  - a key object
- _alist_ - **alist** - list of cons cell with key-value pairs
- _key_   - function for extracting key **before** test
- _test_  - function key and item comparison

ASSOC function searches supplied list for cons cell that have item as car
part. Return value is the cell with key-value pair which key matched testing
conditions, otherwise NIL. **Default comparison operator is EQL.**

Associative list, or for short alist, is a list with key-value pairs in cons
cells. That is:
``` cl
((key1 . value1) (key2 . value2) ...)
```

``` cl
(assoc 'a '((a . 1) (b . 2) (c . 3))) => (A . 1)
(assoc 'x '((a . 1) (b . 2) (c . 3))) => NIL
(assoc 'b '((a . 1) (b . 2) (c . 3) (b . 4))) => (B . 2)
(assoc "b" '(("a" . 1) ("b" . 2))) => NIL
(assoc "b" '(("a" . 1) ("b" . 2)) :test #'equal) => ("b" . 2)
(assoc 7 '((6 . a) (9 . b)) :key #'1+) => (6 . A)
(assoc 5 nil) => NIL
```

### list

(**list** _list\*_) => list

Argument description:
- _list_ - list of objects

LIST function makes new list from arguments.

See also LISTP, LIST\*

``` cl
(list 1 2 3) => (1 2 3)
(list 'a #c(1 2) "moo") => (A #C(1 2) "moo")
(car (list 1 2 3)) => 1
(cdr (list 1 2 3)) => (2 3)
(list) => NIL
(eq (list) nil) => T
(eq (list) '()) => T
(equal (list 1) (cons 1 nil)) => T
(equal (list 1 'a) (cons 1 (cons 'a nil))) => T
(equal (list 1 'a 3) (cons 1 (cons 'a (cons 3 nil)))) => T
(equal (list 1 'a 3) '(1 . (a . (3 . nil)))) => T
(equal '(1 2 3) (list 1 2 3)) => T
```

### make-list

(**make-list** _size_ :initial-element) => list

Argument description:
- _size_ - list size

This creates and returns a list containing _size_ elements, each of which
is initialized to the `:initial-element` argument (which defaults to nil).

_size_ should be a non-negative integer.

``` cl
(make-list 5) => (nil nil nil nil nil)
(make-list 3 :initial-element 'rah) => (rah rah rah)
```

## Chapter-3

### incf

(**incf** _place_ _increment?_) => numeric value

Argument description:
- _place_     -  place with numeric value
- _increment_ -  numeric value

INCF macro modifies a place with numeric value. Its value is incremented by
increment number. **Default increment is 1.**

See also 1+, DECF and 1-

``` cl
(let ((a 10)) (incf a) a) => 11
(let ((a 10)) (incf a 2.3) a) => 12.3
(let ((a 10)) (incf a -2.3) a) => 7.7
(let ((a (list 10 11 12 13))) (incf (elt a 2) 2.3) a) => (10 11 14.3 13)
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
(remove #\s "Sample string sequence") => "Sample tring equence"
(remove #\s "Sample string sequence" :count 1) => "Sample tring sequence"
(remove #\s "Sample string sequence" :test #'char-equal) => "ample tring equence"
(remove nil '(1 2 nil 4 nil 6)) => (1 2 4 6)
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
(every #'characterp "abc") => T
(some #'= '(1 2 3 4 5) '(5 4 3 2 1)) => T (because of 3)
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
(setq y '(1 2 3)) => NIL
(mapc #'print y) => prints 1 2 3 (perform operation on each element)
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
(count #\s "Some sequence") => 1
(count #\s "Some sequence" :key #'char-downcase) => 2
(count #\s "Some sequence" :key #'char-downcase :start 1) => 1
(count #\x "Some sequence") => 0
(count '(1 2) #(9 3 (1 2) 6 7 8)) => 0
(count '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => 1
(count 1 #(0 1 0 0 0 1 0) :from-end t) => 2
```

### delete

(**delete** _item_ _sequence_ :from-end :test :test-not :start :end :count :key) => result-sequence

Omit matching elements. DELETE, DELETE-IF, and DELETE-IF-NOT are like REMOVE,
REMOVE-IF, and REMOVE-IF-NOT respectively, but they may **modify** SEQUENCE.

``` cl
(setq tester (list 1 2 4 1 3 4 5)) => (1 2 4 1 3 4 5)
(delete 4 tester) => (1 2 1 3 5)

(setq tester (list 1 2 4 1 3 4 5)) => (1 2 4 1 3 4 5)
(delete 4 tester :count 1) => (1 2 1 3 4 5)

(setq tester (list 1 2 4 1 3 4 5)) => (1 2 4 1 3 4 5)
(delete 3 tester :test #'>) => (4 3 4 5)

(setq tester (list 1 2 4 1 3 4 5)) => (1 2 4 1 3 4 5)
(delete-if #'oddp tester) => (2 4 4)
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
(find #\s "Some sequence") => #\s
(find #\s "Some sequence" :key #'char-downcase) => #\S
(find #\s "Some sequence" :key #'char-downcase :start 1) => #\s
(find #\x "Some sequence") => NIL
(find '(1 2) #(9 3 (1 2) 6 7 8)) => NIL
(find '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => (1 2)
(find 1 #(0 1 0 0 0 1 0) :from-end t) => 1
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
(position #\s "Some sequence") => 5
(position #\s "Some sequence" :key #'char-downcase) => 0
(position #\s "Some sequence" :key #'char-downcase :start 1) => 5
(position #\x "Some sequence") => NIL
(position '(1 2) #(9 3 (1 2) 6 7 8)) => NIL
(position '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => 2
(position 1 #(0 1 0 0 0 1 0) :from-end t) => 5
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

REDUCE (a.k.a accumulate) applies function fn to its previous result and next element.
The result is what fn returned in last call. For the first call fn is called with either
initial-value and first element or first two elements.

See also MAPCAR, MAPCAN, MAP.

``` cl
(reduce #'list '(1 2 3 4)) => (((1 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0) => ((((0 1) 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0 :from-end t) => (1 (2 (3 (4 0))))
(reduce #'list '(1 2 3 4) :from-end t) => (1 (2 (3 4)))
(reduce (lambda (x y) (+ (* x 10) y)) '(1 2 3 4)) => 1234
(reduce #'+ '(1 2 3 4)) => 10
(reduce #'* '(1 2 3 4) :initial-value 1) => 24
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

NTH may be used to specify a PLACE to SETF. Specifically,
(setf (nth N LIST) NEW-OBJECT) <=> (setf (car (nthcdr N LIST)) NEW-OBJECT)

See also ELT, FIRST, NTHCDR

``` cl
(nth 0 nil) => NIL
(nth 0 '(foo bar baz)) => FOO
(nth 3 '(foo bar baz)) => NIL
```

### reverse

(**reverse** _sequence_) => a sequence

Argument description:
- _sequence_ - a sequence

REVERSE function makes new sequence with reverted order of elements.

See also MAP, MAPCAR and MAPCAN.

``` cl
(reverse '(1 2 3 4)) => (4 3 2 1)
(reverse '#(1 2 3 4)) => #(4 3 2 1)
(reverse "hola") => "aloh"
(reverse nil) => NIL
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
(subseq "hello world" 3) => "lo world"
(subseq "hello world" 3 5) => "lo"
(let ((a "hello world")) (setf (subseq a 3 5) "LO") a) => "helLO world"
(let ((a "hello world")) (setf (subseq a 3 5) "YYY") a) => "helYY world"
```

### aref

(**aref** _array_ _subscripts\*) => element

Argument description:
- _array_      - an array
- _subscripts_ - a list of valid array indices

AREF function accesses specified elements of arrays. Every array index is
counted from zero. Accessing out-of-bounds indices signals condition, or causes
crash and/or undefined behavior, depending on compilation safety mode. **Note that
vectors (including strings which are special vectors) are treated as one
dimensional arrays so aref works on them too.**

AREF with conjunction of SETF may be used to set array elements.

``` cl
(aref "hola" 0) => #\h
(aref "hola" 3) => #\a
(aref #(5 3 6 8) 1) => 3
(aref (make-array '(10 10) :initial-element 'moo) 9 9) => MOO

(let ((a (make-array '(3 3) :initial-element 'moo)))
      (setf (aref a 1 1) 'x) a) => #2A((MOO MOO MOO) (MOO X MOO) (MOO MOO MOO))
````

### char

(**char** _string_ _index_) => character

Argument description:
- _string_ - a string
- _index_  - a valid array index for the string

CHAR access the element of string specified by index. Could be used in SETF

(setf (char string index) new-character)

``` cl
(setq my-simple-string (make-string 6 :initial-element #\A)) =>  "AAAAAA"
(schar my-simple-string 4) =>  #\A
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
(concatenate 'string "hello" " " "world") => "hello world"
(concatenate 'vector "hello" " " "world") => #(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d)
(concatenate 'vector '(1 2) '(3 4)) => #(1 2 3 4)
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
objects. It is not suitable for comparing numbers - see EQL and =. Result is
true if they are same, otherwise false.

``` cl
(eq 'moo 'moo) => T
(eq 1 2) => NIL
(eq 1234567890123456789 1234567890123456789) => NIL
(eq (cons 1 2) (cons 1 2)) => NIL
(let ((x (cons 1 2))) (eq x x)) => T
```

EQL function compares object identity, numbers and characters. Numbers are
considered as equal only when they have the both same value and type. Result is
true if they are same, otherwise false.

``` cl
(eql 'moo 'foo) => NIL
(eql 1 1) => T
(eql 1 2) => NIL
(eql 1234567890123456789 1234567890123456789) => T
(eql 1.0 1) => NIL
(eql 1.0 1.0) => T
(eql (cons 1 2) (cons 1 2)) => NIL
(let ((x (cons 1 2))) (eql x x)) => T
```

EQUAL function compares same things as eql, additionally result is true under
some other situations: conses are compared recursively (in both car and cdr
part), string and bit-vectors are compared element-wise. Result is true if they
are same, otherwise false.

``` cl
(equal #*1010101 #*1010101) = T
(equal (vector 2 3 4) (vector 2 3 4)) = NIL
(equal (cons 1 2) (cons 1 2)) => T
(let ((x (cons 1 2))) (equal x x)) => T
(equal 'moo 'moo) => T
(equal 1.0 1) => NIL
(equal 1.0 1.0) => T
```

EQUALP function compares same things as equal, additionally result is true under
some other situations: conses are compared recursively (in both car and cdr
part), any sequence is compared recursively (element-wise), strings and
characters are compared case insensitively. Result is true if they are same,
otherwise false.

``` cl
(equalp "moo" "MoO") => T
(equalp "moo" "moo ") => NIL
(equalp (cons 1 2) (cons 1 2)) => T
(let ((x (cons 1 2))) (equalp x x)) => T
(equalp 'moo 'moo) => T
(equalp 'moo 'foo) => NIL
(equalp "a" 'a) => NIL
(equalp 1 2) => NIL
(equalp 1.0 1) => T
(equalp 1.0 1.0) => T
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
- _tree_ - a tree represented like list e.g. `((a b) ((c)) (d e))`

COPY-TREE treats passed list as a tree and creates a copy of a tree of conses.
TREE-EQUAL is similar to EQUAL but is more powerful because allows :test keyword.

``` cl
(setf tree '((a b) ((c)) (d e)))
(tree-equal tree (copy-tree tree)) => T
```
### subst
### sublis

(**subst** _new_ _old_ _tree_ :key :test :test-not)<br/>
(**sublis** _(old . new)-alist_ _tree_ :key :test :test-not)

This two functions substitute a new expression for an old one anywhere within
the tree. **subst** substitutes a single value for another, while **sublis**
takes a list of substitutions in the form of an association list of (old . new)
pairs.

_Note_ that the order of old and new in the alist for **sublis** is reversed from
the order of arguments to **subst**.

``` cl
(subst 'new 'old '(old ((very old))) => (NEW ((VERY NEW)))
(sublis '((old . new)) '(old ((very old))))=> (NEW ((VERY NEW)))
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
(rem -1 5) => -1
(mod -1 5) => 4
(mod 13 4) => 1
(rem 13 4) => 1
(mod -13 4) => 3
(rem -13 4) => -1
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
 => (1 4 5 B C D "a" "B" "c" "D")

(intersection list1 list2) => (C B 4 1 1)
(intersection list1 list2 :test 'equal) => ("B" C B 4 1 1)
(intersection list1 list2 :test #'equalp) => ("d" "C" "B" "A" C B 4 1 1)

(nintersection list1 list2) => (1 1 4 B C)
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
(union '(1 2 3) '(2 3 4)) => (1 2 3 4)
(union '((1) (2) (3)) '((2) (3) (4))) => ((3) (2) (1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :test #'equal) => ((1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :key #'first) => ((1) (2) (3) (4))
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
(set-difference '(a b c) '(b c d)) => (A)
(set-difference '("a" "b" "c") '("b" "c" "d")) => ("c" "b" "a")
(set-difference '("a" "b" "c") '("b" "c" "d") :test #'equal) => ("a")
(set-difference '((a . 2) (b . 3) (c . 1))
                '((b . 1) (c . 2) (d . 4)) :key #'car) => ((A . 2))
```

### subsetp

(**subsetp** _list1_ _list2_ :test :test-not :key)<br/>
(**nsubsetp** _list1_ _list2_ :test :test-not :key)

Return true if _list1_ is a subset of _list2_.
I.e., if every element of _list1_ also appears in _list2_.

  (adjoin _item_ _list_ :test :test-not :key)

Return _item_ consed onto the front of _list_ only if it's not already there.
**Otherwise, return _list_ unmodified.**

Set operations with different types:

| lists           | integers   | bit vectors |
|:----------------|------------|-------------|
| intersection    | logand     | bit-and     |
| union           | logior     | bit-ior     |
| set-difference  | logandc2   | bit-andc2   |
| member          | logbitp    | bit         |
| length          | logcount   |             |

``` cl
(intersection '(a bed) '(a b e)) => (A B)
(bit-and #*11110 #*11001) => #*11000
(logand #bllll0 #bll001) => 24 = #bll000
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

### read

(**read** _input-stream?_ _eof-error-p?_ _eof-value?_ _recursive-p_) => an object

Argument description:

- _input-stream_ - an input stream, **default is standard input**
- _eof-error-p_  - a boolean, true (default) is EOF should be signaled
- _eof-value_    - an object that is returned as EOF value
- _recursive-p_  - flag to note recursive processing

READ function reads arbitrary readable lisp object from input stream.
Reading process uses \*read-table\*.

Note that \*read-eval\* global variable controls read-time evaluation (#. macro).

See also: READ-LINE, WRITE and WRITE-LINE

``` cl
(let ((s (make-string-input-stream "(1 2 3)"))) (read s)) => (1 2 3)
(let ((s (make-string-input-stream "#(1 2 3)"))) (read s)) => #(1 2 3)
(let ((s (make-string-input-stream "\"hola\""))) (read s)) => "hola"
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
(let ((x 'x)) (push 4 x) x) => (4 . X)
(let ((x '(3 2 1))) (push 4 x) x) => (4 3 2 1)
(let ((x '((a b c) (3 2 1) (e f g)))) (push 4 (second x)) x) => ((A B C) (4 3 2 1) (E F G))
```

### fresh-line

(**fresh-line** &optional stream) => T or NIL

FRESH-LINE writes a newline unless it can determine that the output stream is already at
the beginning of a new line; FRESH-LINE returns T if it actually wrote a newline, and NIL
otherwise.

## Koans

Taken from **Lisp-koans**

_(work in progress)_
