;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;;; File exercises.lisp

(in-package #:ch2-exercises)

;; Exercise 2.1 [m] Write a version of generate that uses cond but avoids calling
;; rewrites twice.

;; Exercise 2.2 [m] Write a version of generate that explicitly differentiates between
;; terminal symbols (those with no rewrite rules) and nonterminal symbols.

;; Exercise 2.4 [m] Oneway of describing combine-all is that it calculates the cross-
;; product of the function a ppend on the argument lists. Write the higher-order function
;; cross-product, and define combine-all in terms of it.
;;
;; The moral is to make your code as general as possible, because you never know what
;; you may want to do with it next.

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))
