;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH4-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-gps1.lisp

(in-package #:ch4-first)

(defexamples 4.1 "GPS: The General Problem Solver - first version"
  ""
  "The General problem Solver, developed in 1957 by Alan Newell and Herbert"
  "Simon, embodied a grandiose vision: a single computer program that could"
  "solve ANY problem. GPS caused quite a stir ..."
  ""
  "Here are some examples of using GPS"
  ""
  "The first example works with a complex chain of steps."
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(son-at-school)
        *school-ops*) => SOLVED @ 118)
  ""
  "The next example fails because there is no way to make the car work,"
  "because we can't contact the shop to get the battery fixed."
  ((gps '(son-at-home car-needs-battery have-money)
        '(son-at-school)
        *school-ops*) => NIL)
  ""
  "The third example is easy, because the car is currently working."
  ((gps '(son-at-home car-works)
        '(son-at-school)
        *school-ops*) => SOLVED)

  (:section "4.7 The Clobbered Sibling Goal Problem")
  ""
  "In the next example, GPS incorrectly reports success, when in fact it has"
  "spent the money on the battery, and thus should fail."
  ((gps '(son-at-home have-money car-works)
        '(have-money son-at-school)
        *school-ops*) => SOLVED @ 120)
  ""
  "The bug is that when (EVERY #'ACHIEVE GOALS) returns true, it means all the"
  "goals were achieved in turn, but they might not still be all true."

  (:section "4.8 The Leaping before You Look Problem")
  ""
  "What happens if we move the HAVE-MONEY goal to the end?"
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(have-money son-at-school)
        *school-ops*) => SOLVED @ 121)
  ""
  "GPS returns nil, but only after executing all the actions."
  "I call this the 'leaping before you look' problem, because if you asked"
  "the program to solve for the two goals (JUMP-OFF-CLIFF LAND-SAFELY) it"
  "would happily jump first, only to discover that it had no operator to land"
  "safely. This is less than prudent behavior."

  (:section "4.9 The Recursive Subgoal Problem")
  ""
  "We won't show the problem (because it gets into an infinite loop),"
  "but we will add the new operator in final version to the *school-ops*."
  ""
  "((push (make-op :action 'ask-phone-number"
  "                :preconds '(in-communication-with-shop)"
  "                :add-list '(know-phone-number))"
  "       *school-ops*) @ 122)"
  ""
  "NOTE: If you add this code, you can't run examples 4.1 twice."
)
