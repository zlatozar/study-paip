;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:tutor)

(defexamples 3 "Overview of Lisp"
  "This chapter briefly covers the most important special forms and"
  "functions in Lisp."

  (:section "3.2 Special Forms")
  "Start with functions and special forms for repetition:"
  "First, functions like MAPCAR can apply to any number of lists:"

  ((mapcar #'- '(1 2 3)) => (-1 -2 -3) @ 61)
  ((mapcar #'+ '(1 2) '(10 20) '(100 200)) => (111 222))

  "Second, many of the functions accept keywords:"
  ((remove 1 '(1 2 3 2 1 0 -1)) => (2 3 2 0 -1) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :key #'abs) => (2 3 2 0) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :test #'<) => (1 1 0 -1) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :start 4) => (1 2 3 2 0 -1) @ 61)

  "Third, some have corresponding -IF or -IF-NOT versions:"
  ((remove-if #'oddp '(1 2 3 2 1 0 -1)) => (2 2 0))
  ((remove-if-not #'oddp '(1 2 3 2 1 0 -1)) => (1 3 1 -1))

  ;; requires "overview.lisp"

  "The forms TRACE and UNTRACE are used to control debugging info:"
  ((trace length9) @ 65)
  ((length9 '(1 b c)) => 3)
  ((untrace length9))
  ((length9 '(1 b c)) => 3)

  (:section "3.7 Functions on Trees")

  ((defvar tree nil))
  ((setf tree '((a b) ((c)) (d e))) @ 76)
  ((tree-equal tree (copy-tree tree)) => t)
  ((same-shape-tree tree '((1 2) ((3)) (4 5))) => t)
  ((same-shape-tree tree '((1 2) (3) (4 5))) => nil)

  "There are two functions for substituting a new expression into a tree:"
  ((subst 'new 'old '(old ((very old)))) => (NEW ((VERY NEW))))
  ((sublis '((old . new)) '(old ((very old)))) => (NEW ((VERY NEW))))
  ((subst 'new 'old 'old) => NEW)

  "For big example see 'english->french' function @ 77"

  (:section "3.10 Destructive Functions")

  "Consider the following:"
  ((defvar x nil))
  ((defvar y nil))
  ((setq x '(a b c)) @ 80)
  ((setq y '(1 2 3)))
  ((nconc x y) => (a b c 1 2 3))
  (x => (a b c 1 2 3))
  (y => (1 2 3))

  "NCONC computes the same result as APPEND, but it alters the first argument."
  "It is called a 'destructive' function."
  "There is quite a conceptual load on the programmer who uses NCONC."
  "The advantage of NCONC is that it doesn't use any storage."
  ""
  (:section "3.11 Overview of Data Types")
  "The function TYPE-OF returns the type of its argument."

  ((type-of 123) => (integer 0 4611686018427387903) @ 82)
  ((typep 123 'fixnum) => t)
  ((typep 123 'integer) => t)
  ((typep 123.0 'integer) => nil)
  ((subtypep 'fixnum 'integer) => t)

  (:section "3.12 Input/Output")
  "FORMAT is the main function for formatted output:"

  ((format t "hello, world") @ 84)
  ((format t "~&~a plus ~s is ~f" "two" "two" 4))
  ((let ((numbers '( 1 2 3 4 5)))
     (format t "~&~{~r~^ plus ~} is ~@r"
             numbers (apply #'+ numbers))))

  (:section "3.13 Debugging tools")

  ((documentation 'first 'function) @ 87)
  ((documentation 'pi 'variable))

  (:section "3.14 Antibugging Tools")

  ((defun f (n) (dotimes (i n) nil)) @ 90)
  ((time (f 10000)))
  ((compile 'f))
  ((time (f 10000)))

  (:section "3.15 Evaluation")
  "The following five forms are equivalent:"

  ((+ 1 2 3 4) => 10 @ 91)
  ((funcall #'+ 1 2 3 4) => 10 @ 91)
  ((apply #'+ '(1 2 3 4)) => 10 @ 91)
  ((apply #'+ 1 2 '(3 4)) => 10 @ 91)
  ((eval '(+ 1 2 3 4)) => 10 @ 91)

  (:section "3.16 Closures")
  "In the general case, a function consists of the body of the function"
  "coupled with any free lexical variables that the function references."
  "Consider the example:"

  ((mapcar (adder 3) '(1 3 10)) => (4 6 13) @ 92)
  ((mapcar (adder 10) '(1 3 10)) => (11 13 20) @ 92)

  "In the following, two calls to BANK-ACCOUNT create two different closures,"
  "each with a separate value for the lexical variable BALANCE."
  ((defvar my-account nil))
  ((defvar your-account nil))
  ((setf my-account (bank-account 500.00)) @ 92)
  ((setf your-account (bank-account 250.00)) @ 93)

  ((funcall my-account :withdraw 75.00) => 425.0)
  ((funcall your-account :deposit 250.00) => 500.0)
  ((funcall your-account :withdraw 100.00) => 400.0)
  ((funcall my-account :withdraw 25.00) => 400.0)

  "This style of programming is covered in more detail in chapter 13.")
