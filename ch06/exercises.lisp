;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH6-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch6-exercises)

;; Exercise 6.5 [m] Define a version of compose that allows any number of arguments

;; Hint: try to make decisions when compose is called to build the resulting function,
;; rather than making the same decisions over and over each time the resulting function is
;; called.

;;; ____________________________________________________________________________

(defun compose (&rest functions)
  "Return the function that is the composition of all the args.
i.e. (compose f g h) = (lambda (x) (f (g (h x))))."
  (case (length functions)
    (0 #'identity)
    (1 (first functions))
    (2 (let ((f (first functions))
             (g (second functions)))
         #'(lambda (x) (funcall f (funcall g x)))))
    (t #'(lambda (x)
           (reduce #'funcall functions :from-end t
                   :initial-value x)))))

;;; ____________________________________________________________________________

;; Exercise 6.9 [m] The sorter function is inefficient for two reasons: it calls `append',
;; which has to make a copy of the first argument, and it sorts the entire result, rather
;; than just inserting the new states into the already sorted old states. Write a more
;; efficient sorter.

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to COST-FN."
  #'(lambda (new old)
      (merge 'list (sort new #'> :key cost-fn)
             old #'> :key cost-fn)))

;;; ____________________________________________________________________________

;; Exercise 6.11[m] Write a function that calls `ch6:beam-search' to find the first N
;; solutions to a problem and returns them in a list.

(defun search-n (start n goal-p successors cost-fn beam-width)
  "Find N solutions to a search problem, using beam search."
  (let ((solutions nil))
    (beam-search
     start #'(lambda (x)
               (cond ((not (funcall goal-p x)) nil)
                     ((= n 0) x)
                     (t (decf n)
                        (push x solutions)
                        nil)))
     successors cost-fn beam-width)
    solutions))

;;; ____________________________________________________________________________

;; Exercise: Use `ch6:depth-first-search' for a given tree:
;; (0 (1 (2) (3)) (4 (5 (6) NIL) NIL))

(defparameter *bin-tree* '(0 (1 (2) (3)) (4 (5 (6)))))

(defun successors-fn (node)
  "Define legal next moves for a given NODE"
  (rest node))

(defun is= (value)
  "Compare node value with a VALUE"
  #'(lambda (x) (eql (car x) value)))

;; test DFS
(deftest test-depth-first-search ()
  (check
    (equal (depth-first-search *bin-tree* (is= 1)  #'successors-fn)
           '(1 (2) (3)) )))

;;; ____________________________________________________________________________
;;;                                                                    My notes

;; It is important to understand that goal and successors functions should be fast
;; and if possible - easy to be written.
;;
;; If tree is represented as a list there is no way to find out successors if you pass
;; just element (number in our case). That's why node should be passed and we should take
;; care for that as passing tree not just element:
;;
;; (depth-first-search *bin-tree* goal-p successors-fn)
