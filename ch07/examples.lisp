;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH7; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch7)

(defexamples 7 "STUDENT: Solving Algebra Word Problems"
  ""
  "STUDENT was another early language understanding program, written by Daniel"
  "Bobrow in 1964.  It was designed to read and solve the kind of word"
  "problems found in high school algebra books."

  (:section "7.1 Translating English into Equations")
  ""
  ((translate-to-expression '(if z is 3 |,| what is twice z))
   :=> ((= z 3) (= what (* 2 z))) @ 222)

  (:section "7.2 Solving Algebra Equations")
  ""
  ((trace isolate solve) @ 229)
  ((solve-equations '((= (+ 3 4) (* (- 5 (+ 2 x)) 7))
                      (= (+ (* 3 x) y) 12))) :=> nil)
  ((untrace isolate solve))

  (:section "7.3 Examples")
  ""
  ((student '(If the number of customers Tom gets is twice the square of
              20 % of the number of advertisements he runs |,|
              and the number of advertisements is 45 |,|
              then what is the number of customers Tom gets ?)) :=> nil @ 231)

  ((student '(The daily cost of living for a group is the overhead cost plus
              the running cost for each person times the number of people in
              the group |.|  This cost for one group equals $ 100 |,|
              and the number of people in the group is 40 |.|
              If the overhead cost is 10 times the running cost |,|
              find the overhead and running cost for each person |.|)))

  ((student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
              Kelly's IQ minus 80 is Robin's height |.|
              If Robin is 4 feet tall |,| how old is Fran ?)))

  ((student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
              Kelly's IQ minus 80 is Robin's height |.|
              If Robin is 0 feet tall |,| how old is Fran ?)))
  )
