;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH4-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch4-exercises)

;; Exercise 4.2 [m] Write a function that generates all permutations of its input.

;;; ____________________________________________________________________________

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation: nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, elem, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add elem to the front of each of these.
      ;; Do this for all possible elem to generate all permutations.
      (mapcan #'(lambda (elem)
                  (mapcar #'(lambda (permute) (cons elem permute))
                          (permutations
                           (remove elem bag :count 1 :test #'eq))))
              bag)))

;;; TEST

(deftest test-permutations ()
  (check
    (equal (permutations '(a b c)) '((A B C) (A C B) (B A C) (B C A) (C A B) (C B A)))))

;;; ____________________________________________________________________________
;;;                                                                    My notes

;; Let's exemplify. Suppose that we have a bag with tree elements (1 2 3) then
;; the permutations will be: (1 2 3) (2 1 3) (3 1 2)
;;                           (1 3 2) (2 3 1) (3 2 1)
;;
;; What we met here is that there is a pattern - use current element and after it permutation
;; of the cdr of the list. But this is not exactly true - it is not a cdr of the list but
;; permutation of the list without current element. For sure this function will be recursive!
;; Let's sketch the function:

;; (defun permutation (bag)
;;   {base case}
;;   (map #'(lambda (elem)
;;        {some other function}
;;        (permutation (remove elem bag)))
;;   bag))
;;
;; The main question is what this {some other function} will receive. Again exemplify:
;;
;; ( 1                           2                           3)
;; ((1 {permutation without 1}) (2 {permutation without 2}) (3 {permutation with 3}))
;;
;; {some other function} will receive {permutation without elem} so we have to iterate again.
;;
;; (defun permutation (bag)
;;   {base case}
;;   (map #'(lambda (elem)
;;        (map #'(lambda (permute)  ...)
;;             (permutation (remove elem bag))))
;;   bag))
;;
;; Here is the tricky moment - What should do with permute?
;; When you deal with recursive functions it is very useful to ACCEPT that recursion JUST works!
;; In our case we accept that (permutation (remove elem bag)) return {permutation without elem} correctly.
;; permute = {permutation without elem} so we have to form (elem {permutation without elem}):
;;
;; (defun permutation (bag)
;;   {base case}
;;   (map #'(lambda (elem)
;;        (map #'(lambda (permute) (cons elem permute)
;;             (permutation (remove elem bag))))
;;   bag))
;;
;; Base case is easy - permutation of empty bag is just ()
;;
;; (defun permutations (bag)
;;   (if (null bag)
;;       '(())
;;       (map #'(lambda (elem)
;;                   (map #'(lambda (permute) (cons elem permute))
;;                           (permutations
;;                            (remove elem bag :count 1 :test #'eq))))
;;               bag)))
;;
;; 'remove' could be difficult...
;;
;; What is left is to choose 'right' map function:
;; - mapcar - apply function and result is a new list
;; - mapcan - splicing together the results
