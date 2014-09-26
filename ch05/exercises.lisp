;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;;; File exercises.lisp

(in-package #:ch5-exercises)

;; Exercise 5.14[m] Define a version of 'mappend' that, like 'mapcar', accepts any number
;; of argument lists.

;;; ____________________________________________________________________________

(defun mappend (fn &rest lists)
  "Apply fn to each element of lists and append the results."
  (apply #'append (apply #'mapcar fn lists)))

;;; TEST


;;; ____________________________________________________________________________
;;;                                                                    My notes
