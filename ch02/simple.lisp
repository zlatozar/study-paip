;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH2; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(in-package #:ch2)

;;; ____________________________________________________________________________
;;;                                                    Straightforward solution

;; p. 36
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of SET, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;;; ____________________________________________________________________________

;; p. 38
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

;; (defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))

(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))


;; (defun Adj* ()
;;   "Warning - incorrect definition of Adjectives. "
;;   (one-of (list nil (append (Adj) (Adj*)))))
;;
;; It will cause infinite recursion because 'one-of' parameter should be
;; created but this creation never ends.

;;; ____________________________________________________________________________
;;;                                                               Grammar rules

;; Represent a grammar that could be easily interpret. That is the power of Lisp.

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*,
but we can switch to other grammars.")

;;; ____________________________________________________________________________
;;;                                                            Helper functions

;; It is a good idea to impose a layer of abstraction by defining functions to
;; operate on the rules.

;; p. 40
(defun rule-lhs (rule)
  "The left hand side of a RULE."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a RULE."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this CATEGORY."
  (rule-rhs (assoc category *grammar*)))

;;; ____________________________________________________________________________
;;;                                                         Rule-Based Solution

;; The function `generate' is an interpreter for the "language" defined by the set of
;; grammar rules.

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

;;; ____________________________________________________________________________

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

;; (setf *grammar* *bigger-grammar*)

;;; ____________________________________________________________________________

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;;; ____________________________________________________________________________
;;;                                      Use the Same Data for Several Programs

;; p. 44
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this PHRASE."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

;;; See Readme.md for more explanation

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
E.g., (combine-all '((a) (b)) '((1) (2))) -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))
