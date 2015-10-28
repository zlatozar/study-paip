;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH11-FINAL; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-prolog.lisp

(in-package #:ch11-final)

(defexamples 11 "Logic Programming (final implementation)"
  ""
  "The idea behind logic programming is that the programmer should state the"
  "relationships that describe a problem and its solution."
  "In this chapter we develop an interpreter for the Prolog language."

  (:section "11.3 Idea 3: Automatic Backtracking")
  ""
  "Now we load the version that does automatic backtracking one step at a time"
  "as opposed to the previous version, which collects all answers at once."
  "Since we don't want to involve you, the user, in typing input to move on"
  "to the next step, we supply the input (a ; or a .) as in the book."
  "Unfortunately, it is not specified in Common Lisp whether read-char echoes"
  "the character it reads, so you may or may not see the ; and . characters."
  ""
  "Let's add the definition of the relation LENGTH:"

  ((<- (length () 0)) @ 370)
  ((<- (length (?x . ?y) (1+ ?n)) (length ?y ?n)))
  ""
  "Here are some queries:"

  ((?- (length (a b c d) ?n)) :input ";")
  ((?- (length ?list (1+ (1+ 0)))) :input ";")
  ((?- (length ?list ?n)) :input ";;.")
  ((?- (length ?l (1+ (1+ 0))) (member a ?l)) :input ";;")
  ""
  "(We won't try the example that leads to an infinite loop.)"

  (:section "11.4 The Zebra Puzzle")
  ""
  "First we define the NEXTO and IRIGHT (to the immediate right) relations:"

  ((<- (nextto ?x ?y ?list) (iright ?x ?y ?list)) @ 374)
  ((<- (nextto ?x ?y ?list) (iright ?y ?x ?list)))
  ((<- (iright ?left ?right (?left ?right . ?rest))))
  ((<- (iright ?left ?right (?x . ?rest))
       (iright ?left ?right ?rest)))
  ((<- (= ?x ?x)))
  ""
  "Now we define the zebra puzzle:"

  ((<- (zebra ?h ?w ?z)
       ;; Each house is of the form:
       ;; (house nationality pet cigarette drink house-color)

       (= ?h ((house norwegian ? ? ? ?)               ; 1, 10
              ?
              (house ? ? ? milk ?) ? ?))              ; 9
       (member (house englishman ? ? ? red) ?h)       ; 2
       (member (house spaniard dog ? ? ?) ?h)         ; 3
       (member (house ? ? ? coffee green) ?h)         ; 4
       (member (house ukrainian ? ? tea ?) ?h)        ; 5
       (iright (house ? ? ? ? ivory)                  ; 6
               (house ? ? ? ? green) ?h)
       (member (house ? snails winston ? ?) ?h)       ; 7
       (member (house ? ? kools ? yellow) ?h)         ; 8
       (nextto (house ? ? chesterfield ? ?)           ; 11
               (house ? fox ? ? ?) ?h)
       (nextto (house ? ? kools ? ?)                  ; 12
               (house ? horse ? ? ?) ?h)
       (member (house ? ? luckystrike oj ?) ?h)       ; 13
       (member (house japanese ? parliaments ? ?) ?h) ; 14
       (nextto (house norwegian ? ? ? ?)              ; 15
               (house ? ? ? ? blue) ?h)

       (member (house ?w ? ? water ?) ?h)       ; Q1
       (member (house ?z zebra ? ? ?) ?h)))     ; Q2
  ""
  "If you want to test this out, run the following query:"
  "   ((?- (zebra ?houses ?water-drinker ?zebra-owner)))"
  ""
  "NOTE: It is not included as an example because it takes a minute or so to run."
)
