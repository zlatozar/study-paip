;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(in-package #:ch9)

;;; ____________________________________________________________________________
;;;                                                         Code from Chapter 2

(defun rule-lhs (rule)
  "The left hand side of a RULE."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a RULE."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element of `set', and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;;; ____________________________________________________________________________

;; p. 276
(defun compile-rule (rule)
  "Translate a grammar rule into a LISP function definition."
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) `(one-of `,rhs))
              ((length=1 rhs) (build-code (first rhs)))
              (t `(case (random `,(length rhs))
         ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  "Return a list of case-clauses"
  (when choices
    (cons (list number (build-code (first choices)))
          (build-cases (+ number 1) (rest choices)))))

(defun build-code (choice)
  "Append together multiple constituents"
  (cond ((null choice) nil)
        ((atom choice) (list choice))
        ((length=1 choice) choice)
        (t `(append ,@(mapcar #'build-code choice)))))
