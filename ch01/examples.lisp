;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH1; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch1)

(defexamples 1 "Introduction to Lisp"
  ""
  "This chapter is for people with little or no experince in Lisp."
  "Intermediate or advanced readers can skim or skip this chapter."
  ""
  "Lisp expressions are in prefix notation: the operator first."
  ((+ 2 2) => 4 @ 4)
  ((+ 1 2 3 4 5 6 7 8 9 10) => 55 @ 5)
  ""
  "This is Lisp for (900 + 900 + 90 + 9) - (5000 + 500 + 50 + 5)"
  ((- (+ 9000 900 90 9) (+ 5000 500 50 5)) => 4444)
  (:section "1.1 Symbolic Computation")
  ""
  "This is an example of computation on lists:"
  ((append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY) @ 6)
  ""
  "The quote mark instructs Lisp to treat the list as data."
  ('(pat Kim) => (PAT KIM))
  ""
  "Let's look at some more list processing functions"
  (:section "1.4 Lists")
  ""
  ""
  "NOTE: SBCL will warn you: 'undefined variable P'"
  ""
  "The CL standard doesn't define what should happen when you assign a"
  "variable that has not be defined. It leaves it up to the implementation,"
  "and each implementation may do something different."
  ""
  ((setf p '(John Q Public)) @ 10)
  ((first p))
  ((rest p))
  ((second p))
  ((third p))
  ((fourth p))
  ((length p))
  ""
  "It is also possible to build up new lists"
  (p @ 11)
  ((cons 'Mr p))
  ((cons (first p) (rest p)))
  ""
  "NOTE: SBCL will warn you: 'undefined variable TOWN'"
  ""
  ((setf town (list 'Anytown 'USA)))
  ((list p 'of town 'may 'have 'already 'won!))
  ((append p '(of) town '(may have already won)))
  ""
  (:section "1.5 Defining New Functions")
  ""
  "The special form DEFUN stands for 'define function.'"
  "It is used here to define a new function called last-name:"
  ((last-name p) => PUBLIC @ 13)
  ((last-name '(Rex Morgan MD)) => MD)
  ((last-name '(Spot)) => SPOT)
  ((last-name '(Aristotle)) => ARISTOTLE)
  ""
  "We can also define the function first-name."
  "Even though the definition is trivial (it is the same as FIRST),"
  "it is good practice to define first-name explicitly."
  ((first-name p) => JOHN)
  ((first-name '(Wilma Flintstone)) => WILMA)

  ""
  "NOTE: SBCL will warn you: 'undefined variable NAMES'"
  ""
  ((setf names '((John Q Public) (Malcolm X)
                 (Admiral Grace Murray Hopper) (Spot)
                 (Aristotle) (A A Milne) (Z Z Top)
                 (Sir Larry Olivier) (Miss Scarlet))) @ 14)
  ((first-name (first names)) => JOHN)

  (:section "1.6 Using Functions")
  ""
  "Consider the following expression, which can be used to test LAST-NAME:"
  ((mapcar #'last-name names))
  ""
  "The #' notation maps the name of a function to the function itself."
  ((mapcar #'- '(1 2 3 4)) @ 15)
  ((mapcar #'+ '(1 2 3 4) '(10 20 30 40)))
  ""
  "Now that we understand mapcar, let's use it to test FIRST-NAME:"
  ((mapcar #'first-name names))
  ""
  "Suppose we wanted a version of FIRST-NAME that ignored titles like Miss:"
  ((defparameter *titles*
     '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
     "A list of titles that can appear at the start of a name."))
  ""
  "NOTE: SBCL will warn you: STYLE-WARNING: redefining FIRST-NAME in DEFUN"
  ((defun first-name (name)
     "Select the first name from a name represented as a list."
     (if (member (first name) *titles*)
         (first-name (rest name))
         (first name))) @ 16)

  ((mapcar #'first-name names))
  ((first-name '(Madam Major General Paula Jones)) => PAULA)
  ""
  "We can see how this works by tracing the execution of first-name:"
  ((trace first-name))
  ((first-name '(John Q Public)) => JOHN @ 17)
  ((first-name '(Madam Major General Paula Jones)) => PAULA)
  ((untrace first-name))

  (:section "1.7 Higher-Order Functions")
  ""
  ((apply #'+ '(1 2 3 4)) => 10)
  ((apply #'append '((1 2 3) (a b c))))
  ""
  "Now we define a new function, self-and-double, and apply it to arguments."
  ((defun self-and-double (x) (list x (+ x x))))
  ((self-and-double 3) => (3 6))
  ((apply #'self-and-double '(3)) => (3 6))
  ""
  "Now let's return to the mapping functions:"
  ((mapcar #'self-and-double '(1 10 300)))
  ((mappend #'self-and-double '(1 10 300)))
  ""
  "FUNCALL is similar to APPLY; it too takes a function as its"
  "first argument and applies the function to a list of arguments,"
  "but in the case of FUNCALL, the arguments are listed separately:"
  ((funcall #'+ 2 3) => 5 @ 20)
  ((apply #'+ '(2 3)) => 5))
