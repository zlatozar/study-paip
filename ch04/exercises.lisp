;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;;; File exercises.lisp

(in-package #:ch4-exercises)

;; Exercise 4.2 [m] Write a function that generates all permutations of its input.

;;; ____________________________________________________________________________

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                           (remove e bag :count 1 :test #'eq))))
              bag)))

;;; TEST

(deftest test-permutations ()
  (check
    (equal (permutations '(a b c)) '((A B C) (A C B) (B A C) (B C A) (C A B) (C B A)))))

;;; ____________________________________________________________________________
;;;                                                                    My notes

;;; Here is my solutions based on Scheme lectures:
