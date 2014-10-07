;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH1-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch1-exercises)

;; Exercise 1.2 [m] Write a function to exponentiate, or raise a number to an integer
;; power. For example: (power 3 2) = 3^2 = 9.

(defun power (x n)
  "Power raises X to the nth power. N must be an integer >= 0.
This executes in O(log n) time, because of the check for even N."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))

(deftest test-power ()
  (check
    (= (power 3 2) 9)))

;; Exercise 1.3 [m] Write a function that counts the number of atoms in an expression.
;; For example: (count-atoms '(a (b) c)) = 3.
;;
;; Notice that there is something of an ambiguity in this:
;; should (a nil c) count as three atoms, or as two, because it is
;; equivalent to (a () c) ?

(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(deftest test-count-atoms ()
  (check
    (= (count-atoms '(a (b) c)) 3)))

(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression,
counting *nil* as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))

;; Exercise 1.4 [m] Write a function that counts the number of times an expression
;; occurs anywhere within another expression.
;; Example: (count-anywhere 'a '(a ((a) b) a)) => 3.

(defun count-anywhere (item tree)
  "Count the times ITEM appears anywhere within TREE."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))

(deftest test-count-anywhere ()
  (check
    (= (count-anywhere 'a '(a ((a) b) a)) 3)))

;; Exercise 1.5 [m] Write a function to compute the dot product of two sequences
;; of numbers, represented as lists. The dot product is computed by multiplying
;; corresponding elements and then adding up the resulting products.
;; Example: (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))

(deftest test-dot-product ()
  (check
    (= (dot-product '(10 20) '(3 4)) 110)))
