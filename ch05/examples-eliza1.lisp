;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-eliza1.lisp

(in-package #:ch5-first)

(defexamples 5.1 "Eliza: Dialog with a Machine - first version"
  ""
  "ELIZA was one of the first programs to feature English output as well as input."
  "The program was named after the heroine of Pygmalion, who was taught to"
  "speak proper English by a dedicated teacher."

  (:section "5.2 Pattern Matching")
  ""
  "The hard part is the notion of pattern matching and transformation."
  "All symbols beginning with ? are variables for the pattern matcher."
  "First we see how to substitute variable/value pairs into expressions:"
  ((sublis '((?X . vacation)) '(what would it mean to you if you got a ?X ?))
   => (what would it mean to you if you got a VACATION ?) @ 156)
  ""
  "Now a version of pat-match that works with such pairs:"
  ((pat-match '(I need a ?x) '(I need a vacation))  @ 158)
  ""
  "Showing how to plug it in:"
  ((sublis (pat-match '(I need a ?x) '(I need a vacation))
           '(what would it mean to you if you got a ?X ?))
   => (what would it mean to you if you got a VACATION ?) @ 159)
  ((pat-match '(I need a ?x) '(I really need a vacation)) => nil)
  ((pat-match '(this is easy) '(this is easy)) => ((t . t)))
  ((pat-match '(?x is ?x) '((2 + 2) is 4)) => nil)
  ((pat-match '(?x is ?x) '((2 + 2) is (2 + 2))) => ((?x 2 + 2)))
  ((pat-match '(?P need . ?X) '(I need a long vacation))
   => ((?X a long vacation) (?P . I)))

  (:section "5.3 Segment Pattern Matching")
  ""
  "We show how to have a variable that will match more than one element."
  "We call these segment variables, and denote them (?* name)."
  ((pat-match '((?* ?p) need (?* ?x))
              '(Mr Hulot and I need a vacation)) @ 160))
