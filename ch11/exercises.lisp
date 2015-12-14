;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH11-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch11-exercises)

;; Exercise 11.2 [m] Some people find the <- notation difficult to read. Define macros
;; rul e and f a c t so that we can write:
;;
;; (fact (likes Robin cats))
;; (rule (likes Sandy ?x) if (likes ?x cats))

;;; ____________________________________________________________________________

(defmacro defrule (&rest clause)
  `(add-clause ',clause))

(defmacro fact (relation)
  `(defrule ,relation))

;; Also an example how to write macro that contains non keywords
;; 'if' in our case.

(defmacro rule (tail if &rest head)
  (declare (ignore if))
  `(defrule ,tail ,@head))
