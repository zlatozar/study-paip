;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH3-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch3-exercises)

;; Exercise 3.9 [m] Write a version 'of length' using the function 'reduce'.

;;; ____________________________________________________________________________

(defun length-r (list)
  (reduce #'+ list :key #'(lambda (x) (declare-ignore x) 1)))

;;; TEST

(deftest test-length-r ()
  (check
    (= (length-r '(a b c)) 3)))
