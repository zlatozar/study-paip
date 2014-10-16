;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-EXERCISES; Base: 10 -*-

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

;; Exercise 6.9 [m] The sorter function is inefficient for two reasons: it calls append,
;; which has to make a copy of the first argument, and it sorts the entire result, rather
;; than just inserting the new states into the already sorted old states. Write a more
;; efficient sorter.

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to COST-FN."
  #'(lambda (new old)
      (merge 'list (sort new #'> :key cost-fn)
             old #'> :key cost-fn)))

;;; ____________________________________________________________________________

;; Exercise 6.11[m] Write a function that calls 'beam-search' to find the first n solutions
;; to a problem and returns them in a list.

(defun search-n (start n goal-p successors cost-fn beam-width)
  "Find n solutions to a search problem, using beam search."
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
;;;                                                                    My notes
