;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH14-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-krep1.lisp

(in-package #:ch14-first)

(defexamples 14.1 "Knowledge Representation and Reasoning"
  ""
  "In this chapter we explore means of indexing facts so that they can be"
  "retrieved and reasoned with efficiently."
  ""
  "Section 14.1 to 14.7 discuss problems with logical reasoning systems"
  "such as Prolog."
  ""
  (:section "14.8 A Solution to the Indexing Problem")
  ""
  "Here we show how to index facts in a kind of table that makes it easy to"
  "add, delete, and retrieve entries.  We will develop an extension of the"
  "trie or discrimination tree data structure built in section 10.5 (page 344)."
  ""

  "Now we define a function to test the indexing routine.  Compare the output"
  "with figure 14.1 on page 474."
  ""
  ((test-index) @ 478)

  ""
  "Here is an example of fetching from the index"
  ((fetch '(p ? c)) @ 480 =>
   (((P B C) (P A C))
    ((P A ?X))))

  "We can make a change to rename variables before indexing facts."
  ((defun index (key)
     "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
     (dtree-index key (rename-variables key) ; store unique vars
                  (get-dtree (predicate key)))) @ 481)
  ""
  "We have to reindex:"
  ((test-index))

  "We are now ready to test the retrieval mechanism:"
  ""
  ((fetch '(p ?x c)) @ 481)
  ((retrieve '(p ?x c)) @ 481)
  ((retrieve-matches '(p ?x c)) =>
   ((P A C) (P A C) (P B C)))
  ((retrieve-matches '(p ?x (?fn c))) =>
   ((P A (?FN C)) (P A (F C)) (P B (F C))))
  ((query-bind (?x ?fn) '(p ?x (?fn c))
               (format t "~&P holds between ~a and ~a of c." ?x ?fn)) @ 482)
  )
