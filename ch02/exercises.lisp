;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH2-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch2-exercises)

;; Exercise 2.4 [m] Oneway of describing 'combine-all' is that it calculates the cross-
;; product of the function 'append' on the argument lists. Write the higher-order function
;; 'cross-product', and define 'combine-all' in terms of it.

;;; ____________________________________________________________________________

;; The moral is to make your code as general as possible, because you never know what
;; you may want to do with it next.

(defun cross-product (fn xlist ylist)
  "Return a list of all (FN x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))

;;; TEST

(deftest test-combine-all ()
  (check
    (equal (combine-all '((a) (b) (c)) '((1) (2) (3)))
           '((A 1) (B 1) (C 1) (A 2) (B 2) (C 2) (A 3) (B 3) (C 3)))))
