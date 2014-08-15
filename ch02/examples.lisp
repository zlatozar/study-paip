;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch2)

(defexamples 2 "A Simple Lisp Program"

  "This chapter shows how to combine the basic functions and"
  "special forms of Lisp into a complete program"
  "The program generates random English sentences."

  (:section "2.2 A Straightforward Solution")
  "We can test the program by generating a few random sentences."
  "(Note that since these are random, you won't get the same ones"
  "as in the book.)"

  ;; requires "simple.lisp"

  ((sentence) @ 36)
  ((sentence) @ 36)
  ((sentence) @ 36)
  ((noun-phrase))
  ((verb-phrase))
  ((trace sentence noun-phrase verb-phrase article noun verb) @ 37)
  ((sentence))
  ((untrace))

  (:section "2.3 A Rule-Based Solution")
  "An alternative implementation concentrates on making it easy"
  "to write grammar rules."

  ((generate 'sentence) @ 41)
  ((generate 'sentence) @ 41)
  ((generate 'noun-phrase) @ 41)
  ((generate 'verb-phrase) @ 41)

  "One advantage of this approach is its easier to change grammars."
  ((setf *grammar* *bigger-grammar*) @ 43)
  ((generate 'sentence))
  ((generate 'sentence))

  "Another advantage is that the same data (grammar) can be used"
  "for more than one purpose.  Consider generate-tree:"
  ((generate-tree 'sentence) @ 45))
