;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch9)

(defexamples 9 "Efficiency Issues"
  ""
  "One of the reasons Lisp has enjoyed a long history is because it is an"
  "ideal language for what is called rapid-prototyping or rapid development."
  "Most real AI programs deal with large amounts of data. Thus, efficiency"
  "is important. This chapter shows some ways to make programs efficient."

  (:section "9.1 Caching Results of Previous Computations: Memoization")
  ""
  ((defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))) @ 269)
  ((setf memo-fib (memo #'fib)) @ 270)
  ((trace fib))
  ((funcall memo-fib 3) :=> 3 @ 270)
  ((funcall memo-fib 3) :=> 3)
  ((untrace fib))
  ((memoize 'fib) @ 272)
  ((trace fib))
  ((fib 5) :=> 8)
  ((fib 5) :=> 8)
  ((fib 6) :=> 13)
  ((untrace fib))
  )
