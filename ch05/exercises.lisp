;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch5-exercises)

;; Exercise 5.14[m] Define a version of `paip-aux:mappend' that, like `mapcar', accepts any number
;; of argument lists.

;;; ____________________________________________________________________________

;; From PAIP and 'On Lisp'
(defun mappend (fn &rest lists)
  "Apply FN to each element of LISTS and append the results."
  (apply #'append (apply #'mapcar fn lists)))

;; From AIMA
(defun mappend2 (fn &rest lists)
  "Apply FN to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

;;; TEST

(deftest test-mappend ()
  (check
    (equal (mappend #'list '(a b c) '(1 2 3 4)) '(A 1 B 2 C 3))))


;;; ____________________________________________________________________________
;;;                                                                    My notes

;; REMEMBER: `mappend' offers a nondestructive alternative to `mapcan'!

;; Loosely (apply #'mapcar fn lists) is (mapcar fn list1 list2 ... listN) then collect
;; results with `append' or 'reduce/append'
