;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH10-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch10-exercises)

;;; ____________________________________________________________________________

;; Exercise 10.3 [m] Use `reuse-cons' to write a version of `ch10::flatten' that
;; shares as much of its input with its output as possible.

(defun flatten (exp &optional (so-far nil) last-cons)
  "Return a flat list of the atoms in the input.
Ex: (flatten '((a) (b (c) d))) => (a b c d)."
  (cond ((null exp) so-far)
        ((atom exp) (reuse-cons exp so-far last-cons))
        (t (flatten (first exp)
               (flatten (rest exp) so-far exp)
               exp))))
