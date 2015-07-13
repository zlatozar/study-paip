;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH8; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch8)

(defexamples 8 "Symbolic Mathematics: A Simplification Program"
  ""
  "'Symbolic mathematics' is to numerical mathematics as algebra is to"
  "arithmetic: it deals with variables and expressions, not just numbers."
  "This chapter develops a program that simplifies algebraic expressions."
  "We then show that differentiation and even integration can be seen as"
  "special cases of 'simplification.' (Note that we replace calls to the"
  "interactive function SIMPLIFIER with calls to the function SIMP.)"

  (:section "8.2 Simplification Rules")
  ""
  ((simp '(2 + 2)) :=> 4 @ 245)
  ((simp '(5 * 20 + 30 + 7)) :=> 137 )
  ((simp '(5 * x - (4 + 1) * x)) :=> 0 )
  ((simp '(y / z * (5 * x - (4 + 1) * x))) :=> 0 )
  ((simp '((4 - 3) * x + (y / y - 1) * z)) :=> X )
  ((simp '(1 * f(x) + 0)) :=> (F X) )

  (:section "8.3 Associativity and Commutativity")
  ""
  ((simp '(3 * 2 * x)) :=> (6 * X) @ 247)
  ((simp '(2 * x * x * 3)) :=> (6 * (X ^ 2)) )
  ((simp '(2 * x * 3 * y * 4 * z * 5 * 6)) :=> (720 * (X * (Y * Z))) )
  ((simp '(3 + x + 4 + x)) :=> ((2 * X) + 7) )
  ((simp '(2 * x * 3 * x * 4 * (1 / x) * 5 * 6)) :=> (720 * X))

  (:section "8.4 Logs, Trig, and Differentiation")
  ""
  ((simp '(d (x + x) / d x)) :=> 2 @ 250)
  ((simp '(d (a * x ^ 2 + b * x + c) / d x)) :=> ((2 * (A * X)) + B) )
  ""
  "For the next one, note we had an error in the first printing of the book;"
  "the sign was reversed on the (d (u / v) ...) rule."
  ((simp '(d ((a * x ^ 2 + b * x + c) / x) / d x))
   :=> (((X * ((2 * (A * X)) + B)) - ((A * (X ^ 2)) + ((B * X) + C))) /
       (X ^ 2)))
  ((simp '(log ((d (x + x) / d x) / 2))) :=> 0 )
  ((simp '(log(x + x) - log x)) :=> (LOG 2))
  ((simp '(x ^ cos pi)) :=> (1 / X) )
  ""
  "These next two examples were also affected by the (d (u / v) ...) rule."
  ((simp '(d (3 * x + (cos x) / x) / d x))
   :=> ((((X * (- (SIN X))) - (COS X)) / (X ^ 2)) + 3))
  ((simp '(d ((cos x) / x) / d x))
   :=> (((X * (- (SIN X))) - (COS X)) / (X ^ 2)))
  ((simp '(d (3 * x ^ 2 + 2 * x + 1) / d x)) :=> ((6 * X) + 2))
  ((simp '(sin(x + x) ^ 2 + cos(d x ^ 2 / d x) ^ 2)) :=> 1 )
  ((simp '(sin(x + x) * sin(d x ^ 2 / d x) +
           cos(2 * x) * cos(x * d 2 * y / d y))) :=> 1 )

  (:section "8.5 Limits of Rule-Based Approaches")
  ""
  "In this section we return to some examples that pose problems."
  "For the following, we would prefer (2 * (x + y))"
  ((simp '(x + y + y + x)) :=> (X + (Y + (Y + X))))
  ""
  "For the following, we would prefer (7 * X) and (Y + (8 * X)), respectively:"
  ((simp '(3 * x + 4 * x)) :=> ((3 * X) + (4 * X)))
  ((simp '(3 * x + y + x + 4 * x)) :=> ((3 * X) + (Y + (X + (4 * X)))) )
  ""
  "In chapter 15, we develop a new version of the program that handles this problem."

  (:section "8.6 Integration")
  ""
  ((set-simp-fn 'Int #'(lambda (exp)
                         (integrate (exp-lhs exp) (exp-rhs exp)))) @ 258)
  ((simp '(Int x * sin(x ^ 2) d x)) :=> (1/2 * (- (COS (X ^ 2)))) )
  ((simp '(Int ((3 * x ^ 3) - 1 / (3 * x ^ 3)) d x))
   :=> ((3 * ((X ^ 4) / 4)) - (1/3 * ((X ^ -2) / -2))) )
  ((simp '(Int (3 * x + 2) ^ -2/3 d x)) :=> (((3 * X) + 2) ^ 1/3) )
  ((simp '(Int sin(x) ^ 2 * cos(x) d x)) :=> (((SIN X) ^ 3) / 3) )
  ((simp '(Int sin(x) / (1 + cos(x)) d x)) :=> (-1 * (LOG ((COS X) + 1))) )
  ((simp '(Int (2 * x + 1) / (x ^ 2 + x - 1) d x))
   :=> (LOG ((X ^ 2) + (X - 1))) )
  ((simp '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x))
   :=> (8 * ((1/3 * (((X ^ 3) + 2) ^ -2)) / -2)) )
  ((set-simp-fn 'Int
                #'(lambda (exp)
                    (unfactorize
                     (factorize
                      (integrate (exp-lhs exp) (exp-rhs exp)))))) @ 259)
  ((simp '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x))
   :=> (-4/3 * (((X ^ 3) + 2) ^ -2)) )

  )
