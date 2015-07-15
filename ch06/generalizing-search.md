### Search

Let's examine something that many believe to be an even *more* powerful
_computational metaphor_ for thought.

**All thought is SEARCH.**

Suppose we wanted to search for the symbol FOO in some list:
```cl
(setq fooful-list
      '(eggplant lightbulbs cottage-cheese
        oranges foo cactus-berries goop))

(defun foo-search (l)
  "Returns the first FOO found in l, nil if it ain't there."
  (cond ((null l) nil)
        ((equalp (car l) 'foo)
         'foo)
        (t (foo-search (cdr l)))))
```

```cl
(foo-search fooful-list)
--> FOO

(foo-search '(a big list without the foo-word))
--> NIL
```

Now suppose we're not looking for any *particular* item, but just for some item that meets
some condition.  Let's use numbers for a moment, and write a function to search for the
first ODD number in a list.

```cl
(defun odd-search (l)
  "Returns the first odd number found in l, nil if
one ain't there."
  (cond ((null l) nil)
        ((oddp (car l))
         (car l))
        (t (odd-search (cdr l)))))
```

```cl
(odd-search '(2 4 6 7 8 10 11 12))
--> 7

(odd-search '(2 4 6 8 10 12))
--> NIL
```

Now let's _generalize_ the idea by making the predicate (oddp in the case above) an
argument to the search function:

```cl
(defun list-search (l predicate)
  "Returns the first item found in l that satisfies
the predicate, nil if one ain't there."
  (cond ((null l) nil)
        ((funcall predicate (car l))
         (car l))
        (t (list-search (cdr l) predicate))))
```

```cl
(list-search '(2 4 6 7 8 10 11 12) #'oddp)
--> 7

(list-search '(2 4 6 8 10 12) #'oddp)
--> NIL

(list-search '(2 4 6 7 8 10 11 12) #'evenp)
--> 2

(list-search '(2 4 6 7 8 10 11 12)
             #'(lambda (x) (> x 10)))
--> 11
```

By the way, there's a built-in function in Common Lisp that does what our `LIST-SEARCH`
does, but with a few additional features.

```
FIND-IF test sequence &key :key :start :end :from-end

[Function]
returns the first element in the specified portion
of sequence that satisfies test.
```

```cl
(find-if #'(lambda (x) (> x 10))
         '(2 4 6 7 8 10 11 12))
--> 11
```

Now let's generalize the idea to **nested** lists (also called trees):
NOTE: Close to this is `find-anywhere` defined in chapter 8.

```cl
(defun tree-search (l predicate)
  "Returns the first atom found in l that satisfies
the predicate, nil if one ain't there."
  (cond ((null l) nil)
        ((atom l)
         (when (funcall predicate l) l))
        (t (or (tree-search (car l) predicate)
               (tree-search (cdr l) predicate)))))
```

```cl
(tree-search '((2 4 6) (2  8 (7 8)) (10 11 12))
             #'oddp)
--> 7
```

Let's look at the **order** in which atoms are checked:

```cl
(defun depth-search (l predicate)
  "Returns the first atom found in l that satisfies
the predicate, nil if one ain't there."
  (cond ((null l) nil)
        ((atom l)
         (format t "~%checking: ~a" l)
         (when (funcall predicate l) l))
        (t (or (depth-search (car l) predicate)
               (depth-search (cdr l) predicate)))))
```

```cl
(depth-search '((2 4 6) (2  8 (7 8)) (10 11 12))
              #'oddp)

checking: 2
checking: 4
checking: 6
checking: 2
checking: 8
checking: 7
--> 7
```

This search strategy is called "depth-first" search (DFS), because we go as deep as possible as
soon as possible.

Here's another way of accomplishing the same thing:

```cl
(defun depth-search (l predicate)
  "Returns the first atom found in l that satisfies
the predicate, nil if one ain't there."
  (cond ((null l) nil)
        ((atom (car l))
         (format t "~%checking: ~a" (car l))
         (if (funcall predicate (car l))
           (car l)
           (depth-search (cdr l) predicate)))
        (t (depth-search (append (car l)
                                 (cdr l))
                         predicate))))
```

```cl
(depth-search '((2 4 6) (2  8 (7 8)) (10 11 12))
              #'oddp)

checking: 2
checking: 4
checking: 6
checking: 2
checking: 8
checking: 7
--> 7
```

The above function keeps all of the items to be searched on **L**, rather than in the
recursive function-calling stack. Here's the same thing, except we print **L** each time
(rather than the particular item we are checking):

```cl
(defun depth-search (l predicate)
  "Returns the first atom found in l that satisfies
the predicate, nil if one ain't there."
  (format t "~%input list: ~a" l)
  (cond ((null l) nil)
        ((atom (car l))
         (if (funcall predicate (car l))
           (car l)
           (depth-search (cdr l) predicate)))
        (t (depth-search (append (car l)
                                 (cdr l))
                         predicate))))
```

```cl
(depth-search '((2 4 6) (2  8 (7 8)) (10 11 12))
              #'oddp)

input list: ((2 4 6) (2 8 (7 8)) (10 11 12))
input list: (2 4 6 (2 8 (7 8)) (10 11 12))
input list: (4 6 (2 8 (7 8)) (10 11 12))
input list: (6 (2 8 (7 8)) (10 11 12))
input list: ((2 8 (7 8)) (10 11 12))
input list: (2 8 (7 8) (10 11 12))
input list: (8 (7 8) (10 11 12))
input list: ((7 8) (10 11 12))
input list: (7 8 (10 11 12))
--> 7
```

An alternative is "breadth-first" search (BFS), in which we look at everything at the current
level before progressing to deeper levels. A simple way to implement breadth-first search
is to modify the above code for depth-first search in the following way:

Here is the idea:
Instead of putting new elements on the *front* of the list of items to check, put them at
the *end* of the list of items to check. To do this, all we have to do is to switch _"car"_
and _"cdr"_ in the last recursive call of the function:

```cl
(defun breadth-search (l predicate)
  "Returns the first atom found in l that satisfies
the predicate, nil if one ain't there."
  (format t "~%input list: ~a" l)
  (cond ((null l) nil)
        ((atom (car l))
         (if (funcall predicate (car l))
           (car l)
           (breadth-search (cdr l) predicate)))
        (t (breadth-search (append (cdr l)
                                   (car l))
                           predicate))))
```

```cl
(breadth-search '((2 4 6) (2  8 (7 8)) (10 11 12))
                #'oddp)

input list: ((2 4 6) (2 8 (7 8)) (10 11 12))
input list: ((2 8 (7 8)) (10 11 12) 2 4 6)
input list: ((10 11 12) 2 4 6 2 8 (7 8))
input list: (2 4 6 2 8 (7 8) 10 11 12)
input list: (4 6 2 8 (7 8) 10 11 12)
input list: (6 2 8 (7 8) 10 11 12)
input list: (2 8 (7 8) 10 11 12)
input list: (8 (7 8) 10 11 12)
input list: ((7 8) 10 11 12)
input list: (10 11 12 7 8)
input list: (11 12 7 8)
--> 11
```

AHA! We can generalize the idea of search in another way. **Suppose that we don't initially have**
**a list of items to search.** Abstraction, dude! Suppose that we only have an initial
item and a function that provides a new item from an old item:

```cl
(defun gen-search (item predicate gen-fn)
  "Returns the first item generated from item
with gen-fn that satisfies the predicate."
  (cond ((funcall predicate item)
         item)
        (t (gen-search (funcall gen-fn item)
                       predicate
                       gen-fn))))
```

```cl
(gen-search 1
            #'(lambda (x) (> x 23))
            #'1+)
--> 24

(gen-search 1
            #'(lambda (x) (> x 23))
            #'(lambda (x) (+ x 10)))
--> 31
```

Now suppose that our generating function produces *lists* of next-values-to-check. We will
once again have the choice between depth-first and breadth-first strategies.

* The depth-first strategy is to look at the first item of the first list - if it isn't an
answer then generate a new list from that first item, etc., checking later items in the
top list only when you've exhausted the descendants of the first item.
* The breadth-first strategy is to look at all the items in the first list before generating
any of their descendants. Then check all of the items in the new generation before
expanding again, etc.

Depending on the problem, a depth-first or a breadth-first strategy may be better. Other
strategies are possible as well, and are often better.

```cl
(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies GOAL-P. Start with STATES,
and search according to SUCCESSORS and COMBINER."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))
```

```cl
;; Generate successors
(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(binary-tree 1)
--> (2 3)
(binary-tree 2)
--> (4 5)
(binary-tree 23)
--> (46 47)

(depth-first-search 1
                    #'(lambda (x) (> x 20))
                    #'binary-tree)

;; Search: (1)
;; Search: (2 3)
;; Search: (4 5 3)
;; Search: (8 9 5 3)
;; Search: (16 17 9 5 3)
;; Search: (32 33 17 9 5 3)
--> 32
```

AHA! To get breadth-first search we need a different _combiner function_. To make things
clear we define **PREPEND**, which does an **APPEND** backwards:

```cl
(defun prepend (x y)
  "Prepend y to start of x"
  (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search
   (list start) goal-p successors #'prepend))
```

Here's a function that's handy for creating goal functions:

``` cl
(defun is (value)
  #'(lambda (x) (eql x value)))

(funcall (is 23) 22)
--> NIL

(funcall (is 23) (+ 1 22))
--> T

(breadth-first-search 1 (is 12) 'binary-tree)

;; Search: (1)
;; Search: (2 3)
;; Search: (3 4 5)
;; Search: (4 5 6 7)
;; Search: (5 6 7 8 9)
;; Search: (6 7 8 9 10 11)
;; Search: (7 8 9 10 11 12 13)
;; Search: (8 9 10 11 12 13 14 15)
;; Search: (9 10 11 12 13 14 15 16 17)
;; Search: (10 11 12 13 14 15 16 17 18 19)
;; Search: (11 12 13 14 15 16 17 18 19 20 21)
;; Search: (12 13 14 15 16 17 18 19 20 21 22 23)
--> 12

(depth-first-search 1 (is 12) 'binary-tree)

;; Search: (1)
;; Search: (2 3)
;; Search: (4 5 3)
;; Search: (8 9 5 3)
;; Search: (16 17 9 5 3)
;; Search: (32 33 17 9 5 3)
;; Search: (64 65 33 17 9 5 3)
;; Search: (128 129 65 33 17 9 5 3)
;; Search: (256 257 129 65 33 17 9 5 3)
;; Search: (512 513 257 129 65 33 17 9 5 3)
;; Search: (1024 1025 513 257 129 65 33 17 9 5 3)
;; Search: (2048 2049 1025 513 257 129 65 33 17 9 5 3)
;; Search: (4096 4097 2049 1025 513 257 129 65 33 17 9 5 3)
;; ......
Aborted
```

To allow depth-first search to work in this case we have to _limit the depth_ of the
tree. We could use the following substitute for binary-tree, limiting the values to **15**:

```cl
(defun binary-tree-15 (x)
  (remove-if #'(lambda (child) (> child 15))
             (binary-tree x)))
```

```cl
(binary-tree-15 1)
--> (2 3)
(binary-tree-15 2)
--> (4 5)
(binary-tree-15 23)
--> NIL

(depth-first-search 1 (is 12) 'binary-tree-15)

;; Search: (1)
;; Search: (2 3)
;; Search: (4 5 3)
;; Search: (8 9 5 3)
;; Search: (9 5 3)
;; Search: (5 3)
;; Search: (10 11 3)
;; Search: (11 3)
;; Search: (3)
;; Search: (6 7)
;; Search: (12 13 7)
--> 12
```

In PIAP Norvig does something _more general_ - he defines a function `finite-binary-tree`
that takes a size and returns a FUNCTION like `binary-tree-15`, but with the given
size in place of **15**.

```cl
(defun finite-binary-tree (n)
  "Return a successor function that generates a
binary tree with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))
```

```cl
(depth-first-search 1 (is 12) (finite-binary-tree 15))

;; Search: (1)
;; Search: (2 3)
;; Search: (4 5 3)
;; Search: (8 9 5 3)
;; Search: (9 5 3)
;; Search: (5 3)
;; Search: (10 11 3)
;; Search: (11 3)
;; Search: (3)
;; Search: (6 7)
;; Search: (12 13 7)
--> 12
```
Let's practice and see how search can be used to solve problems.
```
The "Towers of Hanoi" problem consists of three pegs and some number of disks that fit
over the pegs. Disks can be moved only from peg to peg, and only one at a time. Disks are
of different sizes, and a bigger disk can never be on top of a smaller disk.
```
Supposing that we have three disks, we might start with a situation like:
```

      |            |            |
     ===           |            |
    =====          |            |
   =======         |            |
---------------------------------------
```

and be asked to produce a situation like:
```

      |            |            |
      |            |           ===
      |            |          =====
      |            |         =======
---------------------------------------
```

We'll describe the game configurations using numbers for the disks **(larger = bigger)** and
lists for the pegs.

In:  `((1 2 3) () ())`
Out: `(() () (1 2 3))`

We need a function that generates *valid* moves from a given game configuration:
```cl
(defun hanoi-moves (config)
  (append (hanoi-moves-from 1 config)
          (hanoi-moves-from 2 config)
          (hanoi-moves-from 3 config)))

(defun smallest (l)
  (unless (null l)
    (apply #'min l)))

(defun hanoi-moves-from (peg config)
  (let ((peg-index (- peg 1)))
    ;; unless nothing's on the peg
    (unless (null (nth peg-index config))
      ;; try to move the smallest disk to all other pegs
      (let ((disk (smallest (nth peg-index config)))
            (other-pegs (remove peg-index '(0 1 2)))
            (new-configs nil))
        (dolist (p other-pegs new-configs)
          ;; OK if other peg is empty or has only bigger disks
          (when (or (null (nth p config))
                    (< disk (smallest (nth p config))))
            (let ((newcon (copy-list config))) ;; create config
              (setf (nth peg-index newcon)
                    (remove disk (nth peg-index newcon)))
              (setf (nth p newcon)
                    (cons disk (nth p newcon)))
              (push newcon new-configs))))))))
```

```cl
(hanoi-moves '((1 2 3)()()))
--> (((2 3) NIL (1)) ((2 3) (1) NIL))

(hanoi-moves '((2 3) (1) ()))
--> (((3) (1) (2)) ((2 3) NIL (1)) ((1 2 3) NIL NIL))
```

```cl
(defun goalp (config)
  (and (null (first config))
       (null (second config))))
```

```cl
(breadth-first-search '((1 2 3)()())
                      #'goalp
                      #'hanoi-moves)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((2 3) (1) NIL) ((3) (2) (1)) ((2 3) (1) NIL) ((1 2 3) NIL NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL) ((1 2 3) NIL NIL) ((3) (1) (2))
            ((2 3) NIL (1)) ((1 2 3) NIL NIL))
;; Search: (((2 3) (1) NIL) ((1 2 3) NIL NIL) ((3) (1) (2)) ((2 3) NIL (1))
            ((1 2 3) NIL NIL) ((2 3) NIL (1)) ((3) (1 2) NIL) ((1 3) (2) NIL))
;; Search: (((1 2 3) NIL NIL) ((3) (1) (2)) ((2 3) NIL (1)) ((1 2 3) NIL NIL)
            ((2 3) NIL (1)) ((3) (1 2) NIL) ((1 3) (2) NIL) ((3) (1) (2))
            ((2 3) NIL (1)) ((1 2 3) NIL NIL))
;; Search: ...
Aborted
```

While this would eventually find a solution, note how many _redundant states_ are being
added to the queue.

```cl
(defun prepend-no-dups (x y)
  (append y (set-difference x y :test #'equalp)))

(defun breadth-first-search (start goal-p successors)
  (tree-search
   (list start) goal-p successors #'prepend-no-dups))
```

```cl
(breadth-first-search '((1 2 3)()())
                      #'goalp
                      #'hanoi-moves)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) (1) NIL) ((2 3) NIL (1)))
;; Search: (((2 3) NIL (1)) ((1 2 3) NIL NIL) ((3) (1) (2)))
;; Search: (((1 2 3) NIL NIL) ((3) (1) (2)) ((2 3) (1) NIL) ((3) (2) (1)))
;; Search: (((3) (1) (2)) ((2 3) (1) NIL) ((3) (2) (1)) ((2 3) NIL (1)))
;; Search: (((2 3) (1) NIL) ((3) (2) (1)) ((2 3) NIL (1)) ((1 3) NIL (2))
            ((3) NIL (1 2)))
;; Search: (((3) (2) (1)) ((2 3) NIL (1)) ((1 3) NIL (2)) ((3) NIL (1 2))
            ((1 2 3) NIL NIL) ((3) (1) (2)))
[stuff deleted]
--> (NIL NIL (1 2 3))
```

This was better, and I actually let it run to completion, but note _that previously_
_explored states are still being added to the queue._

```cl
(defvar *seen-before* nil)

(defun prepend-never-seen (x y)
  (let ((result
         (append y (set-difference x *seen-before*
                                   :test #'equalp))))
    (setq *seen-before* (append *seen-before* x))
    result))

(defun breadth-first-search (start goal-p successors)
  (setq *seen-before* nil)
  (tree-search
   (list start) goal-p successors #'prepend-never-seen))
```

```cl
(breadth-first-search '((1 2 3)()()) #'goalp #'hanoi-moves)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) (1) NIL) ((2 3) NIL (1)))
;; Search: (((2 3) NIL (1)) ((1 2 3) NIL NIL) ((3) (1) (2)))
;; Search: (((1 2 3) NIL NIL) ((3) (1) (2)) ((3) (2) (1)))
;; Search: (((3) (1) (2)) ((3) (2) (1)))
;; Search: (((3) (2) (1)) ((1 3) NIL (2)) ((3) NIL (1 2)))
;; Search: (((1 3) NIL (2)) ((3) NIL (1 2)) ((1 3) (2) NIL) ((3) (1 2) NIL))
;; Search: (((3) NIL (1 2)) ((1 3) (2) NIL) ((3) (1 2) NIL))
;; Search: (((1 3) (2) NIL) ((3) (1 2) NIL) (NIL (3) (1 2)))
;; Search: (((3) (1 2) NIL) (NIL (3) (1 2)))
;; Search: ((NIL (3) (1 2)) (NIL (1 2) (3)))
;; Search: ((NIL (1 2) (3)) ((1) (3) (2)) (NIL (1 3) (2)))
;; Search: (((1) (3) (2)) (NIL (1 3) (2)) ((1) (2) (3)) (NIL (2) (1 3)))
;; Search: ((NIL (1 3) (2)) ((1) (2) (3)) (NIL (2) (1 3)) ((1) (2 3) NIL))
;; Search: (((1) (2) (3)) (NIL (2) (1 3)) ((1) (2 3) NIL) ((2) (1 3) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2 3) NIL) ((2) (1 3) NIL) ((1) NIL (2 3)))
;; Search: (((1) (2 3) NIL) ((2) (1 3) NIL) ((1) NIL (2 3)) ((2) NIL (1 3)))
;; Search: (((2) (1 3) NIL) ((1) NIL (2 3)) ((2) NIL (1 3)) (NIL (1 2 3) NIL)
            (NIL (2 3) (1)))
;; Search: (((1) NIL (2 3)) ((2) NIL (1 3)) (NIL (1 2 3) NIL) (NIL (2 3) (1))
            ((1 2) (3) NIL) ((2) (3) (1)))
;; Search: (((2) NIL (1 3)) (NIL (1 2 3) NIL) (NIL (2 3) (1)) ((1 2) (3) NIL)
            ((2) (3) (1)) (NIL (1) (2 3)) (NIL NIL (1 2 3)))
;; Search: ((NIL (1 2 3) NIL) (NIL (2 3) (1)) ((1 2) (3) NIL) ((2) (3) (1))
            (NIL (1) (2 3)) (NIL NIL (1 2 3)) ((1 2) NIL (3)) ((2) (1) (3)))
;; Search: ((NIL (2 3) (1)) ((1 2) (3) NIL) ((2) (3) (1)) (NIL (1) (2 3))
            (NIL NIL (1 2 3)) ((1 2) NIL (3)) ((2) (1) (3)))
;; Search: (((1 2) (3) NIL) ((2) (3) (1)) (NIL (1) (2 3)) (NIL NIL (1 2 3))
            ((1 2) NIL (3)) ((2) (1) (3)))
;; Search: (((2) (3) (1)) (NIL (1) (2 3)) (NIL NIL (1 2 3)) ((1 2) NIL (3))
            ((2) (1) (3)))
;; Search: ((NIL (1) (2 3)) (NIL NIL (1 2 3)) ((1 2) NIL (3)) ((2) (1) (3)))
;; Search: ((NIL NIL (1 2 3)) ((1 2) NIL (3)) ((2) (1) (3)))
--> (NIL NIL (1 2 3))
```

Here's a more elegant way to do this, without the _global variable._

```cl
(defun graph-search (states goal-p successors combiner
                            &optional
                            (state= #'eql) old-states)
  "Find a state that satisfies goal-p.  Start with
states, and search according to successors and
combiner. Don't try the same state twice."
  (format t "~&;; Search: ~a" states)
  (cond ((null states) nil)
        ((funcall goal-p (first states))
         (first states))
        (t (graph-search
            (funcall
             combiner
             (new-states states successors
                         state= old-states)
             (rest states))
            goal-p successors combiner state=
            (adjoin (first states) old-states
                    :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen
before."
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)
           (member state old-states :test state=)))
   (funcall successors (first states))))

(defun breadth-first-search (start goal-p successors)
  (graph-search
   (list start) goal-p successors #'prepend #'equalp))
```

```cl
(breadth-first-search '((1 2 3)()())
                      #'goalp
                      #'hanoi-moves)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((2 3) (1) NIL) ((3) (2) (1)))
;; Search: (((3) (2) (1)) ((3) (1) (2)))
;; Search: (((3) (1) (2)) ((3) (1 2) NIL) ((1 3) (2) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL) ((3) NIL (1 2)) ((1 3) NIL (2)))
;; Search: (((1 3) (2) NIL) ((3) NIL (1 2)) ((1 3) NIL (2)) (NIL (1 2) (3)))
;; Search: (((3) NIL (1 2)) ((1 3) NIL (2)) (NIL (1 2) (3)))
;; Search: (((1 3) NIL (2)) (NIL (1 2) (3)) (NIL (3) (1 2)))
;; Search: ((NIL (1 2) (3)) (NIL (3) (1 2)))
;; Search: ((NIL (3) (1 2)) (NIL (2) (1 3)) ((1) (2) (3)))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)) (NIL (1 3) (2)) ((1) (3) (2)))
;; Search: (((1) (2) (3)) (NIL (1 3) (2)) ((1) (3) (2)) ((2) NIL (1 3)))
;; Search: ((NIL (1 3) (2)) ((1) (3) (2)) ((2) NIL (1 3)) ((1) NIL (2 3)))
;; Search: (((1) (3) (2)) ((2) NIL (1 3)) ((1) NIL (2 3)) ((2) (1 3) NIL))
;; Search: (((2) NIL (1 3)) ((1) NIL (2 3)) ((2) (1 3) NIL) ((1) (2 3) NIL))
;; Search: (((1) NIL (2 3)) ((2) (1 3) NIL) ((1) (2 3) NIL) ((2) (1) (3))
            ((1 2) NIL (3)))
;; Search: (((2) (1 3) NIL) ((1) (2 3) NIL) ((2) (1) (3)) ((1 2) NIL (3))
            (NIL NIL (1 2 3)) (NIL (1) (2 3)))
;; Search: (((1) (2 3) NIL) ((2) (1) (3)) ((1 2) NIL (3)) (NIL NIL (1 2 3))
            (NIL (1) (2 3)) ((2) (3) (1)) ((1 2) (3) NIL))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)) (NIL NIL (1 2 3)) (NIL (1) (2 3))
            ((2) (3) (1)) ((1 2) (3) NIL) (NIL (2 3) (1)) (NIL (1 2 3) NIL))
;; Search: (((1 2) NIL (3)) (NIL NIL (1 2 3)) (NIL (1) (2 3)) ((2) (3) (1))
            ((1 2) (3) NIL) (NIL (2 3) (1)) (NIL (1 2 3) NIL))
;; Search: ((NIL NIL (1 2 3)) (NIL (1) (2 3)) ((2) (3) (1)) ((1 2) (3) NIL)
            (NIL (2 3) (1)) (NIL (1 2 3) NIL))
--> (NIL NIL (1 2 3))
```

```cl
(defun depth-first-search (start goal-p successors)
  (graph-search
   (list start) goal-p successors #'append #'equalp))
```

```cl
(depth-first-search '((1 2 3)()())
                    #'goalp
                    #'hanoi-moves)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (1 2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: (((2) NIL (1 3)) ((1) (2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)) ((1) (2) (3)) ((1 3) (2) NIL)
            ((2 3) (1) NIL))
;; Search: ((NIL (1) (2 3)) ((1 2) NIL (3)) ((1) (2) (3)) ((1 3) (2) NIL)
            ((2 3) (1) NIL))
;; Search: ((NIL NIL (1 2 3)) ((1) NIL (2 3)) ((1 2) NIL (3)) ((1) (2) (3))
            ((1 3) (2) NIL) ((2 3) (1) NIL))
--> (NIL NIL (1 2 3))
```

AHA! We can do better searches if we have some idea about which partial solutions are
better candidates for further exploration.

Assuming that the partial solutions most worth exploring are those closest to the target
number, a smart way to search is to use a combiner function that sorts the search queue by
the difference from the target.

```cl
(graph-search '(1) (is 12) 'binary-tree
              #'(lambda (new old)
                  (sort (append new old)
                        #'(lambda (n1 n2)
                            (< (abs (- 12 n1))
                               (abs (- 12 n2)))))))

;; Search: (1)
;; Search: (3 2)
;; Search: (7 6 2)
;; Search: (14 15 6 2)
;; Search: (15 6 2 28 29)
;; Search: (6 2 28 29 30 31)
;; Search: (12 13 2 28 29 30 31)
--> 12
```

Note that this is better than:

```cl
(graph-search '(1) (is 12) 'binary-tree #'prepend)

;; Search: (1)
;; Search: (2 3)
;; Search: (3 4 5)
;; Search: (4 5 6 7)
;; Search: (5 6 7 8 9)
;; Search: (6 7 8 9 10 11)
;; Search: (7 8 9 10 11 12 13)
;; Search: (8 9 10 11 12 13 14 15)
;; Search: (9 10 11 12 13 14 15 16 17)
;; Search: (10 11 12 13 14 15 16 17 18 19)
;; Search: (11 12 13 14 15 16 17 18 19 20 21)
;; Search: (12 13 14 15 16 17 18 19 20 21 22 23)
--> 12
```

The general idea of _examining the most promising paths first_, where "promising" is
"distance from the goal", as assessed by some heuristic function, is called "best first
search." PAIP tools make it easy to use best-first search.

```cl
(defun diff (num)
  "Return the function that finds the difference
from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according
to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors
                                cost-fn)
  "Search lowest cost states first until goal is
reached."
  (graph-search (list start) goal-p successors
                (sorter cost-fn) #'equalp))
```

```cl
(best-first-search 1 (is 12)
                   'binary-tree (diff 12))

;; Search: (1)
;; Search: (3 2)
;; Search: (7 6 2)
;; Search: (14 15 6 2)
;; Search: (15 6 2 28 29)
;; Search: (6 2 28 29 30 31)
;; Search: (12 13 2 28 29 30 31)
--> 12
```
One idea for a heuristic distance function for the "Towers of Hanoi" problem is to count the
number of disks in the wrong place:

```cl
(defun hanoi-diff (goal)
  "Return the function that finds the difference
between a Towers of Hanoi state and the goal"
  #'(lambda (state)
      (+ (abs (- (length (first goal))
                 (length (first state))))
         (abs (- (length (second goal))
                 (length (second state))))
         (abs (- (length (third goal))
                 (length (third state)))))))
```

```cl
(best-first-search '((1 2 3)()())
                   #'goalp
                   #'hanoi-moves
                   (hanoi-diff '(()()(1 2 3))))

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (1 2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: (((2) NIL (1 3)) ((1) (2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)) ((1) (2) (3)) ((1 3) (2) NIL)
            ((2 3) (1) NIL))
;; Search: ((NIL (1) (2 3)) ((1 2) NIL (3)) ((1) (2) (3)) ((1 3) (2) NIL)
            ((2 3) (1) NIL))
;; Search: ((NIL NIL (1 2 3)) ((1) NIL (2 3)) ((1 2) NIL (3)) ((1) (2) (3))
            ((1 3) (2) NIL) ((2 3) (1) NIL))
--> (NIL NIL (1 2 3))
```

```cl
(defun beam-search (start goal-p successors cost-fn
                          beam-width)
  "Search highest scoring states first until goal is
reached, but never consider more than beam-width states at a time."
  (graph-search
   (list start) goal-p successors
   #'(lambda (old new)
       (let ((sorted (funcall (sorter cost-fn)
                              old new)))
         (if (> beam-width (length sorted))
           sorted
           (subseq sorted 0 beam-width))))
   #'equalp))
```

```cl
(beam-search '((1 2 3)()())
             #'goalp
             #'hanoi-moves
             (hanoi-diff '(()()(1 2 3)))
             1)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)))
;; Search: (((3) (2) (1)))
;; Search: (((3) (1 2) NIL))
;; Search: ((NIL (1 2) (3)))
;; Search: ((NIL (2) (1 3)))
;; Search: (((2) NIL (1 3)))
;; Search: (((2) (1) (3)))
;; Search: ((NIL (1) (2 3)))
;; Search: ((NIL NIL (1 2 3)))
--> (NIL NIL (1 2 3))

(beam-search '((1 2 3)()())
             #'goalp
             #'hanoi-moves
             (hanoi-diff '(()()(1 2 3)))
             2)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL))
;; Search: ((NIL (1 2) (3)) ((1 3) (2) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)))
;; Search: (((2) NIL (1 3)) ((1) (2) (3)))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)))
;; Search: ((NIL (1) (2 3)) ((1 2) NIL (3)))
;; Search: ((NIL NIL (1 2 3)) ((1) NIL (2 3)))
--> (NIL NIL (1 2 3))

(beam-search '((1 2 3)()())
             #'goalp
             #'hanoi-moves
             (hanoi-diff '(()()(1 2 3)))
             3)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (1 2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)) ((1 3) (2) NIL))
;; Search: (((2) NIL (1 3)) ((1) (2) (3)) ((1 3) (2) NIL))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)) ((1) (2) (3)))
;; Search: ((NIL (1) (2 3)) ((1 2) NIL (3)) ((1) (2) (3)))
;; Search: ((NIL NIL (1 2 3)) ((1) NIL (2 3)) ((1 2) NIL (3)))
--> (NIL NIL (1 2 3))
```

Here's a **different** heuristic function - it takes the sizes of the disks into account.

```cl
(defun hanoi-diff (goal)
  "Return the function that finds the difference
between a Towers of Hanoi state and the goal"
  (let ((biggest (apply #'max (apply #'append goal))))
  #'(lambda (state)
      (let ((penalty 100)(score 0))
        (dotimes (disk biggest)
          (unless (same-position disk state goal)
            (incf score penalty)
            (setq penalty (/ penalty 2))))
        score))))

(defun same-position (disk state1 state2)
  (cond ((null state1) nil)
        ((member disk (first state1))
         (member disk (first state2)))
        (t (same-position disk
                          (cdr state1)
                          (cdr state2)))))
```

```cl
(beam-search '((1 2 3)()())
             #'goalp
             #'hanoi-moves
             (hanoi-diff '(()()(1 2 3)))
             3)

;; Search: (((1 2 3) NIL NIL))
;; Search: (((2 3) NIL (1)) ((2 3) (1) NIL))
;; Search: (((3) (2) (1)) ((2 3) (1) NIL))
;; Search: (((3) (1 2) NIL) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (1 2) (3)) ((1 3) (2) NIL) ((2 3) (1) NIL))
;; Search: ((NIL (2) (1 3)) ((1) (2) (3)) ((1 3) (2) NIL))
;; Search: (((2) NIL (1 3)) ((1) (2) (3)) ((1 3) (2) NIL))
;; Search: (((2) (1) (3)) ((1 2) NIL (3)) ((1) (2) (3)))
;; Search: ((NIL (1) (2 3)) ((1 2) NIL (3)) ((1) (2) (3)))
;; Search: ((NIL NIL (1 2 3)) ((1) NIL (2 3)) ((1 2) NIL (3)))
--> (NIL NIL (1 2 3))
```

### The game

_You and I alternatively select numbers between 1 and 9 (inclusive). Each number can_
_only be chosen once. The first person to have among their numbers exactly 3 that sum to 15_
_wins._

It was easy for me to play because I had a tic-tac-toe board with the numbers arranged on
it in such a way that all of the triples that sum to 15 (and only these triples) are
3-in-a-row. So I played by playing tic-tac-toe, not by doing arithmetic.  There's a
general lesson here about the influence of representation on the difficulty of a
problem. The search application we'll look at is: how do we find the number arrangements
with the required property?

- First we need a goal-testing function:

```cl
(defun tic-tac-toe-all-wins-sum-to-15-property (nine-nums)
  "Assuming that the 9 numbers are laid out in the following order:
1 2 3
4 5 6
7 8 9
this predicate returns true if every 3-in-a-row sums to 15."
  (let ((sq1 (nth 0 nine-nums)) (sq2 (nth 1 nine-nums))
        (sq3 (nth 2 nine-nums)) (sq4 (nth 3 nine-nums))
        (sq5 (nth 4 nine-nums)) (sq6 (nth 5 nine-nums))
        (sq7 (nth 6 nine-nums)) (sq8 (nth 7 nine-nums))
        (sq9 (nth 8 nine-nums)))
    (= 15
       (+ sq1 sq2 sq3) (+ sq1 sq5 sq9) (+ sq1 sq4 sq7)
       (+ sq2 sq5 sq8) (+ sq3 sq5 sq7) (+ sq3 sq6 sq9)
       (+ sq4 sq5 sq6) (+ sq7 sq8 sq9))))
```
- Now we need a "successors" function to produce a new set of game boards from a game
board that is not a goal:

```cl
(defun all-swaps (list)
  "Returns a list of all possible versions of list in which
one element has been swapped with one other."
  (let ((answer nil))
    (dotimes (index1 (length list))
      (dotimes (index2 (length list))
        (unless (eql index1 index2)
          (let ((new-list (swap-elts index1 index2 list)))
            (unless (member new-list answer :test #'equalp)
              (push new-list answer))))))
    answer))

(defun insert-at-index (thing index list)
  "Returns list with thing replacing the indexth element."
  (cond ((null list) nil)
        ((< index 1) (cons thing (cdr list)))
        (t (cons (car list)
                 (insert-at-index
                  thing (- index 1) (cdr list))))))
```

```cl
(insert-at-index 'pooch 0 '(fish frog cat spark-plug))
--> (POOCH FROG CAT SPARK-PLUG)

(insert-at-index 'pooch 1 '(fish frog cat spark-plug))
--> (FISH POOCH CAT SPARK-PLUG)

(insert-at-index 'pooch 23 '(fish frog cat spark-plug))
--> (FISH FROG CAT SPARK-PLUG)
```

```cl
(defun swap-elts (index1 index2 list)
  "Returns list with the elements at index1 and index2 swapped."
  (insert-at-index (nth index1 list)
                   index2
                   (insert-at-index (nth index2 list)
                                    index1
                                    list)))
```

```cl
(swap-elts 0 8 '(a b c d e f g h i))
--> (I B C D E F G H A)

(swap-elts 3 4 '(a b c d e f g h i))
--> (A B C E D F G H I)

(all-swaps '(a b c d))
--> ((A B D C) (A D C B) (A C B D)
     (D B C A) (C B A D) (B A C D))
```

Although we are now in a position to try PAIP `breadth-first` and `depth-first` functions,
they don't work very well in this search space. So we'll go ahead and define a cost
function:

```cl
(defun board-cost (nine-nums)
  "Returns a number indicating (heuristically) how costly it will
be to transform the board represented by nine-nums into a goal board."
  (let ((sq1 (nth 0 nine-nums)) (sq2 (nth 1 nine-nums))
        (sq3 (nth 2 nine-nums)) (sq4 (nth 3 nine-nums))
        (sq5 (nth 4 nine-nums)) (sq6 (nth 5 nine-nums))
        (sq7 (nth 6 nine-nums)) (sq8 (nth 7 nine-nums))
        (sq9 (nth 8 nine-nums)) (cost 0))
    (unless (eql (+ sq1 sq2 sq3) 15) (incf cost))
    (unless (eql (+ sq1 sq5 sq9) 15) (incf cost))
    (unless (eql (+ sq1 sq4 sq7) 15) (incf cost))
    (unless (eql (+ sq2 sq5 sq8) 15) (incf cost))
    (unless (eql (+ sq3 sq5 sq7) 15) (incf cost))
    (unless (eql (+ sq3 sq6 sq9) 15) (incf cost))
    (unless (eql (+ sq4 sq5 sq6) 15) (incf cost))
    (unless (eql (+ sq7 sq8 sq9) 15) (incf cost))
    cost))
```

- Now we can define and test a top-level search function using best-first search:

```cl
(defun isometric-game-board ()
  "Returns a permutation of numbers which reveals the
isomorphism of tic tac toe to the game described as follows:
9 cards numbered 1 through 9 on a table - 2 people alternate
picking cards.  First to get 3 that sum to 15 wins."
  (best-first-search '(1 2 3 4 5 6 7 8 9)
               #'tic-tac-toe-all-wins-sum-to-15-property
               #'all-swaps
               #'board-cost))
```

```cl
(time (isometric-game-board))
--> (2 9 4 7 5 3 6 1 8)
```

Beam search with a beam width of 1 works in this case as well, and produces an answer
faster:

```cl
(defun isometric-game-board ()
  "Returns a permutation of numbers which reveals the
isomorphism of tic tac toe to the game described as follows:
9 cards numbered 1 through 9 on a table - 2 people alternate
picking cards.  First to get 3 that sum to 15 wins."
  (beam-search '(1 2 3 4 5 6 7 8 9)
               #'tic-tac-toe-all-wins-sum-to-15-property
               #'all-swaps
               #'board-cost
               1))
```

```cl
(time (isometric-game-board))
--> (2 9 4 7 5 3 6 1 8)
```

A modified version of PAIP *search-all* function can be used to obtain multiple
solutions:

```cl
(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;; Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (beam-search
      start #'(lambda (x)
                (when (funcall goal-p x) (print x))
                nil)
      successors cost-fn beam-width)
    solutions))

(defun isometric-game-board ()
  "Returns a permutation of numbers which reveals the
isomorphism of tic tac toe to the game described as follows:
9 cards numbered 1 through 9 on a table - 2 people alternate
picking cards.  First to get 3 that sum to 15 wins."
  (search-all '(1 2 3 4 5 6 7 8 9)
              #'tic-tac-toe-all-wins-sum-to-15-property
              #'all-swaps
              #'board-cost
              1))
```

```cl
(isometric-game-board)

(2 9 4 7 5 3 6 1 8)
(4 9 2 3 5 7 8 1 6)
(6 7 2 1 5 9 8 3 4)
(2 7 6 9 5 1 4 3 8)
(8 1 6 3 5 7 4 9 2)
(6 1 8 7 5 3 2 9 4)
(4 3 8 9 5 1 2 7 6)
(8 3 4 1 5 9 6 7 2)
Aborted
```

So far we've looked at heuristic functions that estimate the distance from some search
state to a solution. In many cases we not only searching for a goal state, but we are
looking for the shortest overall path from the start state to the goal state. **A*** is a
search algorithm that can find the optimal path.
