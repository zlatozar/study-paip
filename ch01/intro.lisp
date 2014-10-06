;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH1; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File intro.lisp: Miscellaneous functions from the book introduction.

(in-package #:ch1)

;;; ____________________________________________________________________________
;;;                                                                   Functions

;; p. 12
(defun last-name (name)
  "Select the last name from a `name' represented as a list."
  (first (last name)))

(defun first-name (name)
  "Select the first name from a `name' represented as a list."
  (first name))

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  "Select the first name from a `name' represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

;;; ____________________________________________________________________________
;;;                                                        High order functions

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

;;; ____________________________________________________________________________

;; p. 31
(defun atomprint (exp &optional (depth 0))
  "Print each atom in `exp', along with its depth of nesting."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))
