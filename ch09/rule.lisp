;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File rule.lisp

(in-package #:ch9)

(defparameter *grammar*
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

;;; ____________________________________________________________________________

;; It is possible but not handy (also SBCL warnings):
;;
;; (dolist (rule *grammar*) (eval (compile-rule rule)))
;; (dolist (rule *grammar*) (compile (eval (compile-rule rule))))

;; One frequent way to use compilation is to define a macro that expands into the code
;; generated by the compiler. That way, we just type in calls to the macro and don't have to
;; worry about making sure all the latest rules have been compiled.

(defmacro defrule (&rest rule)
  "Define a grammar rule"
  (compile-rule rule))

;; Better usage (reorder them to avoid SBCL warnings):

;; (defrule Art -> the a)
;; (defrule Noun -> man ball woman table)
;; (defrule Verb -> hit took saw liked)
;; (defrule NP -> (Art Noun))
;; (defrule VP -> (Verb NP))
;; (defrule Sentence -> (NP VP))

;; Then just like in Chapter 2
;; CL-USER> (sentence)

;; See the process:
;; (compile-rule '(Noun -> man ball woman table))
;; (compile-rule '(Sentence -> (NP VP)))
;; (compile-rule '(defrule Adj* -> () Adj (Adj Adj*)))

;; or in other way
;; (macroexpand-1 '(defrule Adj* -> () Adj (Adj Adj*)))
