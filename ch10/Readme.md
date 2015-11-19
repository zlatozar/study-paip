## Chapter 10

What happens when you already are using the best imaginable algorithms, and performance is
still a problem? One answer is to find what parts of the program are used most frequently
and make micro-optimizations to those parts. This chapter covers the following six
optimization techniques.

    * Use declarations.
    * Avoid generic functions.
    * Avoid complex argument lists.
    * Provide compiler macros.
    * Avoid unnecessary consing.
    * Use the right data structure.

#### Use declarations

On general-purpose computers running Lisp, much time is spent on type-checking. You can
gain efficiency at the cost of robustness by declaring, or _promising_, that certain
variables will always be of a given type.

You should declare a function ```inline``` when it is short and the function-calling
overhead will thus be a significant part of the total execution time. You should not
declare a function ```inline``` when the function is recursive, when its definition is
likely to change, or when the function's definition is long and it is called from many
places.

#### Avoid generic functions

- Avoid Complex Argument Lists

#### Avoid unnecessary consing

There are actually two relevant measures of the amount of space consumed by a program: the
amount of storage allocated, and the amount of storage retained (garbage collection).
_Garbage collection is particularly worrisome for real-time systems, because it can happen_
_at any time._

- Use ```nconc```, ```nreverse``` and ```delete```
- The most common kind of unnecessary copying can be eliminated by simple reorganization of your code!
- Use vectors instead of lists

    * In general, vectors take half as much space as lists to store the same information, since
    half of every list is just pointing to the next element.

    * When a vector is created, it can be given a fill pointer. This is a counter
    variable, but one that is conceptually stored inside the vector. Vectors with fill
    pointers act like a cross between a vector and a stack. You can push new elements onto the
    stack with the functions ```vector-push``` or ```vector-push-extend```.

    * Replacing lists with vectors can often save garbage.
    But when you must use lists, it pays to use a version of **cons** that avoids consing when
    possible.

- Reuse values rather than creating copies

- For what ```reuse-cons``` is good for?

Here is "standard" definition of `remq` using consing:
```cl
(defun remq (item list)
  (cond ((null list) nil)
	((eq item (first list)) (remq item (rest list)))
    ;; we always consing
    (t (cons (first list) (remq item (rest list))))))
```

At first sight it is not big optimization if we use `reuse-cons`:
```cl
(defun remq (item list)
  (cond ((null list) nil)
	((eq item (first list)) (remq item (rest list)))
	(t (reuse-cons (first list)
		       (remq item (rest list))
		       list))))
```
Think twice! **What happen if recursive call returns the same list(no elements areremoved)?**

Let's change code a little bit:
```cl
(defun reuse-cons (x y x-y)
  "Return (cons X Y), or reuse X-Y if it is equal to (cons X Y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      (progn
        (princ "x-y")
        x-y)
      (cons x y)))
```
Here is the result if remove only once? ```trace``` is a great tool!
```cl
PAIP> (remq 1 '(1 2 3 4  5 6 7  8 9 10))
  0: (REUSE-CONS 10 NIL (10))
x-y  0: REUSE-CONS returned (10)
  0: (REUSE-CONS 9 (10) (9 10))
x-y  0: REUSE-CONS returned (9 10)
  0: (REUSE-CONS 8 (9 10) (8 9 10))
x-y  0: REUSE-CONS returned (8 9 10)
  0: (REUSE-CONS 7 (8 9 10) (7 8 9 10))
x-y  0: REUSE-CONS returned (7 8 9 10)
  0: (REUSE-CONS 6 (7 8 9 10) (6 7 8 9 10))
x-y  0: REUSE-CONS returned (6 7 8 9 10)
  0: (REUSE-CONS 5 (6 7 8 9 10) (5 6 7 8 9 10))
x-y  0: REUSE-CONS returned (5 6 7 8 9 10)
  0: (REUSE-CONS 4 (5 6 7 8 9 10) (4 5 6 7 8 9 10))
x-y  0: REUSE-CONS returned (4 5 6 7 8 9 10)
  0: (REUSE-CONS 3 (4 5 6 7 8 9 10) (3 4 5 6 7 8 9 10))
x-y  0: REUSE-CONS returned (3 4 5 6 7 8 9 10)
  0: (REUSE-CONS 2 (3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10))
x-y  0: REUSE-CONS returned (2 3 4 5 6 7 8 9 10)
(2 3 4 5 6 7 8 9 10)
```
Of course it is not big optimization if remove frequently. Try this:
```cl
PAIP> (remq 1 '(1 2 1 3 1 4 5 1 6 1 7 8 1 9 1 10))
```

#### Resource pools

Sometimes it pays to manage explicitly the storage of instances of some data type. A pool
of these instances may be called a resource. Explicit management of a resource
is appropriate when:

1. Instances are frequently created, and are needed only temporarily
2. It is easy/possible to be sure when instances are no longer needed
3. Instances are fairly large structures or take a long time to initialize, so that it
is worth reusing them instead of creating new ones.

## Variables

```cl
;; 'find-package' also searches in package nicknames
(defun make-variable () (gentemp "X" #.(find-package "KEYWORD")))
```

The reader character sequence ```#.``` means to evaluate at read time, rather than at
execution time. _"read-time eval"_ means, that it would be evaluated each time the code
itself is read. In lisp, when we want to define a non-lisp syntax, it doesn't make sense
to use the lisp reader - that is only for reading lisp. The read macro is the device we
use to process non-lisp syntax before the lisp reader gets its paws on it. Meaning that
you have turned the syntax into lisp.

#### Data structures

_Lisp is a language that pushes you towards thinking in terms of tree-shaped_
_data-structures and recursive algorithms rather than array/matrix shaped data-structures_
_and for-loop based iteration._

**The key point in TRIE implementation is that tree could be represented as list and**
**list could be represented with cons cells!**

Here is how tree(nested list) could linearized:

```
(a (b c) d)
=> (cons a (cons (cons b (cons c nil)) (cons d nil)))
=> (cons a cons cons b cons c nil cons d nil)
```
In the **trie** implementation ```cons``` are represented with ```"."```.
Let's add some data and ```trace``` the ```find-trie```:

```cl

(setf a-trie (make-trie))
(trace find-trie)

(put-trie '(a) a-trie 10)
(put-trie '(a b) a-trie 20)

;; See the structure of the tree
(get-trie '(a b) a-trie)
```

Remember that instead of ```cons``` we use ```"."``` and instead of
```A``` for example a structure is used (because we want to store value)

Here is the result and how could be interpreted.
```
;; Find (A B) in (cons 'A (cons 'B nil)). We start with cons
0: (FIND-TRIE (A B) NIL
  #S(CH10::TRIE
     :VALUE NIL
     :ARCS (("."
              . #S(CH10::TRIE
                  :VALUE NIL
                  :ARCS ((A
                          . #S(CH10::TRIE
                               :VALUE NIL
                               :ARCS (("."
                                       . #S(CH10::TRIE
                                            :VALUE NIL
                                            :ARCS ((B
                                                    . #S(CH10::TRIE
                                                         :VALUE NIL
                                                         :ARCS ((NIL
                                                                 . #S(CH10::TRIE
                                                                      :VALUE 20
                                                                      :ARCS NIL))))))))
                                      (NIL
                                       . #S(CH10::TRIE
                                            :VALUE 10
                                            :ARCS NIL)))))))))))
;; Gave me everything after first cons
1: (FIND-TRIE "." NIL
  #S(CH10::TRIE
     :VALUE NIL
     :ARCS (("."
             . #S(CH10::TRIE
                  :VALUE NIL
                  :ARCS ((A
                          . #S(CH10::TRIE
                               :VALUE NIL
                               :ARCS (("."
                                       . #S(CH10::TRIE
                                            :VALUE NIL
                                            :ARCS ((B
                                                    . #S(CH10::TRIE
                                                         :VALUE NIL
                                                         :ARCS ((NIL
                                                                 . #S(CH10::TRIE
                                                                      :VALUE 20
                                                                      :ARCS NIL))))))))
                                      (NIL
                                       . #S(CH10::TRIE
                                            :VALUE 10
                                            :ARCS NIL)))))))))))
;; Here is everything after first cons
1: FIND-TRIE returned
     #S(CH10::TRIE
        :VALUE NIL
        :ARCS ((A
                . #S(CH10::TRIE
                     :VALUE NIL
                     :ARCS (("."
                             . #S(CH10::TRIE
                                  :VALUE NIL
                                  :ARCS ((B
                                          . #S(CH10::TRIE
                                               :VALUE NIL
                                               :ARCS ((NIL
                                                       . #S(CH10::TRIE
                                                            :VALUE 20
                                                            :ARCS NIL))))))))
                            (NIL . #S(CH10::TRIE :VALUE 10 :ARCS NIL)))))))

;; Search for A in a arc that starts with A
1: (FIND-TRIE A NIL
              #S(CH10::TRIE
                 :VALUE NIL
                 :ARCS ((A
                         . #S(CH10::TRIE
                              :VALUE NIL
                              :ARCS (("."
                                      . #S(CH10::TRIE
                                           :VALUE NIL
                                           :ARCS ((B
                                                   . #S(CH10::TRIE
                                                        :VALUE NIL
                                                        :ARCS ((NIL
                                                                . #S(CH10::TRIE
                                                                     :VALUE 20
                                                                     :ARCS NIL))))))))
                                     (NIL
                                      . #S(CH10::TRIE
                                           :VALUE 10
                                           :ARCS NIL))))))))
;; Here is the arc after A
1: FIND-TRIE returned
     #S(CH10::TRIE
        :VALUE NIL
        :ARCS (("."
                . #S(CH10::TRIE
                     :VALUE NIL
                     :ARCS ((B
                             . #S(CH10::TRIE
                                  :VALUE NIL
                                  :ARCS ((NIL
                                          . #S(CH10::TRIE
                                               :VALUE 20
                                               :ARCS NIL))))))))
               (NIL . #S(CH10::TRIE :VALUE 10 :ARCS NIL))))

;; Search B in (cons 'B nil)
1: (FIND-TRIE (B) NIL
              #S(CH10::TRIE
                 :VALUE NIL
                 :ARCS (("."
                         . #S(CH10::TRIE
                              :VALUE NIL
                              :ARCS ((B
                                      . #S(CH10::TRIE
                                           :VALUE NIL
                                           :ARCS ((NIL
                                                   . #S(CH10::TRIE
                                                        :VALUE 20
                                                        :ARCS NIL))))))))
                        (NIL . #S(CH10::TRIE :VALUE 10 :ARCS NIL)))))

  ;; After cons please
  2: (FIND-TRIE "." NIL
                #S(CH10::TRIE
                   :VALUE NIL
                   :ARCS (("."
                           . #S(CH10::TRIE
                                :VALUE NIL
                                :ARCS ((B
                                        . #S(CH10::TRIE
                                             :VALUE NIL
                                             :ARCS ((NIL
                                                     . #S(CH10::TRIE
                                                          :VALUE 20
                                                          :ARCS NIL))))))))
                          (NIL . #S(CH10::TRIE :VALUE 10 :ARCS NIL)))))

  ;; Here is B
  2: FIND-TRIE returned
       #S(CH10::TRIE
          :VALUE NIL
          :ARCS ((B
                  . #S(CH10::TRIE
                       :VALUE NIL
                       :ARCS ((NIL
                               . #S(CH10::TRIE :VALUE 20 :ARCS NIL)))))))

  ;; It is atom please give me its value
  2: (FIND-TRIE B NIL
                #S(CH10::TRIE
                   :VALUE NIL
                   :ARCS ((B
                           . #S(CH10::TRIE
                                :VALUE NIL
                                :ARCS ((NIL
                                        . #S(CH10::TRIE
                                             :VALUE 20
                                             :ARCS NIL))))))))

... return from recursion ...
20
T
```
