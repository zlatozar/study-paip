;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH10; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch10)

(defexamples 10 "Low-Level Efficiency Issues"
  ""
  "The efficiency techniques of the previous chapter all involved fairly"
  "significant changes to an algorithm.  But what happens when you are already"
  "using the best imaginable algorithms, and performance is still a problem?"

  (:section "10.1 Use Declarations")
  ""
  "Compare these functions with and without declarations:"
  ""
  ((defun f (x y)
     (declare (fixnum x y) (optimize (safety 0) (speed 3)))
     (the fixnum (+ x y))) @ 318)
  ((defun g (x y) (+ x y)))
  ""
  "Here is the disassembled code for f and g:"
  ""
  ((disassemble 'f))
  ((disassemble 'g) @ 319)
  )
