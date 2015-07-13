;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH6; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch6)

(defexamples 6 "Building Software Tools"
  ""
  "In chapters 4 and 5 we were concerned with buildinng two particular"
  "programs, GPS and ELIZA. In this chapter, we will reexamine those"
  "two programs to discover some common patterns. Those patterns will be"
  "abstracted out to form reusable software tools."

  (:section "6.2 A Pattern-Matching tool (patmatch.lisp)")
  ""
  ((pat-match '(x = (?is ?n numberp)) '(x = 34)) :=> ((?n . 34)) @ 179)
  ((pat-match '(x = (?is ?n numberp)) '(x = x)) :=> NIL)
  ((pat-match '(?x (?or < = >) ?y) '(3 < 4)) :=> ((?Y . 4) (?X . 3)))
  ((pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3))
   :=> ((?N . 3)))
  ((pat-match '(?x /= (?not ?x)) '(3 /= 4)) :=> ((?X . 3)) @ 180)
  ((pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) :=> ((?Y . 3) (?X . 4)))
  ((pat-match '(a (?* ?x) d) '(a b c d)) :=> ((?X B C)) @ 185)
  ((pat-match '(a (?* ?x) (?* ?y) d) '(a b c d)) :=> ((?Y B C) (?X)))
  ((pat-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d)))
   :=> ((?Y D) (?X B C)) @ 186)
  ((pat-match '(?x ?op ?y is ?z (?if (eql (funcall ?op ?x ?y) ?z)))
              '(3 + 4 is 7))
   :=> ((?Z . 7) (?Y . 4) (?OP . +) (?X . 3)))
  ((pat-match '(?x ?op ?y (?if (funcall ?op ?x ?y))) '(3 > 4)) :=> NIL)
  ((pat-match-abbrev '?x* '(?* ?x)) :=> (?* ?X) @ 187)
  ((pat-match-abbrev '?y* '(?* ?y)) :=> (?* ?Y))
  ""
  "NOTE: SBCL will warn you: 'undefined variable AXYD'"
  ""
  ((setf axyd (expand-pat-match-abbrev '(a ?x* ?y* d)))
   :=> (A (?* ?X) (?* ?Y) D))
  ((pat-match axyd '(a b c d)) :=> ((?Y B C) (?X)))
  ((pat-match '(((?* ?x) (?* ?y)) ?x ?y) '((a b c d) (a b) (c d)))
   :=> NIL)

  (:section "6.4 A Set of Searching Tools (eliza-pm.lisp and search.lisp)")
  ""
  "Debug search"
  ((debug :search) @ 192)
  ""
  "We can search through the binary tree, looking for, say, 12 as the goal."
  "With breadth-first search this would yield an infinite loop, so we won't"
  "do it. Breadth-first search works better:"
  ((breadth-first-search 1 (is 12) 'binary-tree) :=> 12 @ 193)
  ((depth-first-search 1 (is 12) (finite-binary-tree 15)) :=> 12 @ 193)

  ""
  "Guiding the Search"
  ""
  "Best-first search takes an additional argument which estimates how close"
  "we are to the goal. We call this the cost function."
  ((best-first-search 1 (is 12) #'binary-tree (diff 12)) :=> 12 @ 195)
  ((best-first-search 1 (is 12) #'binary-tree (price-is-right 12)) :=> 12)
  ""
  "The function beam-search is just like best-first-search, except that after"
  "we sort the states, we then take only the first beam-width states."
  ((beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2) :=> 12)
  ""
  "As a concrete example of a problem that can be solved by search,"
  "consider planning a flight across North America in a plane whose range is"
  "limited to 1000 kilometers. Here we plan a trip from SF to Boston."
  ((path-state (trip (city 'san-francisco) (city 'boston)))
   :=> (BOSTON 71.05 42.21) @ 199)
  ((path-state (trip (city 'boston) (city 'san-francisco)))
   :=> (SAN-FRANCISCO 122.26 37.47))
  ""
  "Undebug search"
  ((undebug :search))

  ((show-city-path (trip (city 'san-francisco) (city 'boston) 1)) @ 201)
  ((show-city-path (trip (city 'boston) (city 'san-francisco) 1)))
  ((show-city-path (trip (city 'boston) (city 'san-francisco) 3)) @ 202)
  ((iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))  :=> 12 @ 205)
  ((tree-search '(1) (is 6) #'next2 #'prepend) :=> 6 @ 208)
  ((graph-search '(1) (is 6) #'next2 #'prepend) :=> 6)
  ((path-states
    (a*-search (list (make-path :state 1)) (is 6)
               #'next2 #'(lambda (x y)
                           (declare-ignore x)
                           (declare-ignore y) 1) (diff 6))) :=> (6 5 3 1) @ 210)

  (:section "6.5 GPS as Search (gps-srch.lisp)")

  ""
  "ATTENTION"
  "Following tests use symbols and they are not suitable"
  "for project that use packages"
  ""

  "((setf start '((c on a) (a on table) (b on table) (space on c)
                 (space on b) (space on table))) @ 213)"

  "((use (make-block-ops '(a b c))) :=> 18)"

  "((search-gps start '((a on b) (b on c)))
   :=> ((START)
       (EXECUTING (MOVE C FROM A TO TABLE))
       (EXECUTING (MOVE B FROM TABLE TO C))
       (EXECUTING (MOVE A FROM TABLE TO B))) @ 213)"
  "((search-gps start '((b on c) (a on b)))
   :=> ((START)
       (EXECUTING (MOVE C FROM A TO TABLE))
       (EXECUTING (MOVE B FROM TABLE TO C))
       (EXECUTING (MOVE A FROM TABLE TO B))))"
  )
