;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-gps.lisp

(in-package #:ch4-final)

(defexamples 4 "GPS: The General Problem Solver - final version"

  (:section "4.11 GPS Version 2: A More General problem Solver")

  "At this point we are ready to put together a new version of GPS with"
  "solutions for the 'running around the block,' 'prerequisite clobbers"
  "sibling goal,' 'leaping before you look,' and 'recursive subgoal' problems."
  "The most important change is that, instead of printing a message when each"
  "operator is applied, we will instead have GPS return the resulting state."

  "We use the list of operators that includes the 'asking the shop their"
  "phone number' operator."
  ((push (make-op :action 'ask-phone-number
                  :preconds '(in-communication-with-shop)
                  :add-list '(know-phone-number))
         *school-ops*))
  ((use *school-ops*) => 7 @ 130)

  "First we make sure the new version works on some of the examples that"
  "version 1 worked on:"
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(son-at-school)) =>
   ((START)
    (EXECUTING LOOK-UP-NUMBER)
    (EXECUTING TELEPHONE-SHOP)
    (EXECUTING TELL-SHOP-PROBLEM)
    (EXECUTING GIVE-SHOP-MONEY)
    (EXECUTING SHOP-INSTALLS-BATTERY)
    (EXECUTING DRIVE-SON-TO-SCHOOL)) @ 131)

  "We can see what is going on here by turning on debugging temporarily:"
  ((debug :gps))
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(son-at-school)) =>
   ((START)
    (EXECUTING LOOK-UP-NUMBER)
    (EXECUTING TELEPHONE-SHOP)
    (EXECUTING TELL-SHOP-PROBLEM)
    (EXECUTING GIVE-SHOP-MONEY)
    (EXECUTING SHOP-INSTALLS-BATTERY)
    (EXECUTING DRIVE-SON-TO-SCHOOL)) @ 131)
  ((undebug))

  "Here is another old example:"
  ((gps '(son-at-home car-works)
        '(son-at-school)) =>
   ((START)
    (EXECUTING DRIVE-SON-TO-SCHOOL)) @ 132)

  "Now we see that version 2 can handle the three cases version 1 got wrong."
  "In each case the program avoids an infinite loop, and also avoids leaping"
  "before it looks."
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(have-money son-at-school)) => NIL)
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
        '(son-at-school have-money)) => NIL)
  ((gps '(son-at-home car-needs-battery have-money)
        '(son-at-school)) => NIL)
  "Finally, we see the new GPS also works on trivial problems:"
  ((gps '(son-at-home) '(son-at-home)) => ((START)))

  (:section "4.12 The New Domain Problem: Monkey and Bananas")

  "To show that GPS is at all general, we have to make it work in different"
  "domains.  We start with a 'classic' AI problem: Monkey and Bananas"
  ((use *banana-ops*) => 6 @ 133)

  "We pose the problem of becoming not-hungry, given an initial state."
  "GPS can find a solution to this problem:"
  ((GPS '(at-door on-floor has-ball hungry chair-at-door)
        '(not-hungry)) =>
   ((START)
    (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
    (EXECUTING CLIMB-ON-CHAIR)
    (EXECUTING DROP-BALL)
    (EXECUTING GRASP-BANANAS)
    (EXECUTING EAT-BANANAS)) @ 133)
  "Notice we did not need to make any changes at all to the GPS program."
  "We just used a different set of operators."

  (:section "4.13 The Maze Searching Domain")

  "Next we will consider another 'classic' problem, maze searching."
  "We will assume a particular maze, diagrammed on page 134."
  ((use *maze-ops*) => 48 @ 134)
  ((gps '((at 1)) '((at 25))) @ 135)

  "We can define FIND-PATH to use the results of a GPS search:"
  ((find-path 1 25) @ 136 =>
   (1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25))
  ((find-path 1 1) => (1))
  ((equal (find-path 1 25) (reverse (find-path 25 1))) => T)

  (:section "4.14 The Blocks World Domain")

  "Another domain that has attracted more than its share of attention in AI"
  "circles is the blocks world domain."
  ((use (make-block-ops '(a b))) => 4 @ 137)
  "The simplest possible problem is stacking one block on another."
  ((gps '((a on table) (b on table) (space on a) (space on b)
          (space on table))
        '((a on b) (b on table))) =>
   ((START)
    (EXECUTING (MOVE A FROM TABLE TO B))))

  "Here is a slightly more complex problem: inverting a stack of two blocks."
  "This time we show the debugging output:"
  ((debug :gps) @ 138)
  ((gps '((a on b) (b on table) (space on a) (space on table))
        '((b on a))) =>
   ((START)
    (EXECUTING (MOVE A FROM B TO TABLE))
    (EXECUTING (MOVE B FROM TABLE TO A))))
  ((undebug))

  "Now we move on to the three block world."
  ((use (make-block-ops '(a b c))) => 18)

  "We try some problems:"
  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
        '((b on a) (c on b))) =>
   ((START)
    (EXECUTING (MOVE A FROM B TO TABLE))
    (EXECUTING (MOVE B FROM C TO A))
    (EXECUTING (MOVE C FROM TABLE TO B))))
  ((gps '((c on a) (a on table) (b on table)
          (space on c) (space on b) (space on table))
        '((c on table) (a on b))) =>
   ((START)
    (EXECUTING (MOVE C FROM A TO TABLE))
    (EXECUTING (MOVE A FROM TABLE TO B))) @ 141)
  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
        '((b on a) (c on b))) @ 141 =>
   ((START)
    (EXECUTING (MOVE A FROM B TO TABLE))
    (EXECUTING (MOVE B FROM C TO A))
    (EXECUTING (MOVE C FROM TABLE TO B))))

  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
        '((c on b) (b on a))) =>
   ((START)
    (EXECUTING (MOVE A FROM B TO TABLE))
    (EXECUTING (MOVE B FROM C TO A))
    (EXECUTING (MOVE C FROM TABLE TO B))))

  ""
  "NOTE: SBCL will warn you: 'undefined variable'"
  ""
  "The CL standard doesn't define what should happen when you assign a"
  "variable that has not be defined. It leaves it up to the implementation,"
  "and each implementation may do something different."
  ""
  "The Sussman Anomaly."
  ((setf start '((c on a) (a on table) (b on table) (space on c)
                 (space on b) (space on table))) @ 142)
  ((gps start '((a on b) (b on c))) => NIL)
  ((gps start '((b on c) (a on b))) => NIL)

  (:section "4.16 The Not Looking after You Don't Leap Problem")
  ((use (push (op 'taxi-son-to-school
                  :preconds '(son-at-home have-money)
                  :add-list '(son-at-school)
                  :del-list '(son-at-home have-money))
              *school-ops*)) @ 143)
  ((debug :gps))
  ((gps '(son-at-home have-money car-works)
        '(son-at-school have-money)) => NIL)
  ((undebug)))
