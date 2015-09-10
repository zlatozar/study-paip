;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(in-package #:ch9)

;;; ____________________________________________________________________________
;;;                                                         Code from Chapter 2

;; The program we will develop in this chapter generates random English sentences.

(defun rule-lhs (rule)
  "The left hand side of a RULE."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a RULE."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element of SET, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;;; ____________________________________________________________________________
;;;                                                How to write rules compiler?

;;
;; The result should be a function but how it should look like?
;;
;; The simple case is when compile atoms (Noun -> man ball woman table)
;; it should return:
;;
;; (defun Noun ()
;;    (one-of '(man ball woman table)))
;;
;; Note that `one-of' also works if list with one element is passed.
;;
;; What if RHS is a list that contains "complex" elements?
;; For example (PP -> (Prep noun-phrase))
;;
;; Obviously we have to append results:
;;
;; (defun PP ()
;;      (append (Prep) (noun-phrase)))
;;
;; Do not forget that 'Prep' and 'noun-phrase' are functions.
;;
;; Last case is when we have more than one list e.g.
;; (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
;;
;; Function should apply previous two rules for every list. That's why MAPCAR and
;; recursion will do the job. Also we randomly choose between them.  As previous case we
;; have to append but `one-of' is not good choice because parameters will be evaluated. So
;; the best choice is CASE - ideal way to insert code.
;;

;; p. 276
;; Note the comma before COND. It means that COND will be executed and only
;; particular case will be in resulted function.

(defun compile-rule (rule)
  "Translate a grammar rule into a Lisp function definition"
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) `(one-of ',rhs))
	      ((length=1 rhs)
	       (build-code (first rhs)))
	      (t `(case (random ,(length rhs))
		    ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  (when choices
    (cons (list number (build-code (first choices)))
	  (build-cases (1+ number) (rest choices)))))

;; Note: constituent - being a part of a whole
(defun build-code (choice)
  "Append together multiple constituents"
  (cond ((null choice) nil)
	((atom choice) (list choice))
        ;; return code that appends every `build-code' of CHOICE list
	(t `(append ,@(mapcar #'build-code choice)))))
