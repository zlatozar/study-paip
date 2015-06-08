;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH3-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch3-exercises)

;; Exercise 3.3 [m] Write a function that will print an expression in dotted pair
;; notation. Use the built-in function pri nc to print each component of the expression.

;;; ____________________________________________________________________________

(defun dprint (x)
  "Print an expression in dotted pair notation."
  (cond ((atom x) (princ x))
        (t (princ "(")
           (dprint (first x))
           (pr-rest (rest x))
           (princ ")")
           x)))

(defun pr-rest (x)
  (princ " . ")
  (dprint x))


;; Exercise 3.4 [m] Write a function that, like the regular print function, will print an
;; expression in dotted pair notation when necessary but will use normal list notation
;; when possible.

;;; ____________________________________________________________________________

(defun pr-rest (x)
  (cond ((null x))
        ((atom x) (princ " . ") (princ x))
        (t (princ " ") (dprint (first x)) (pr-rest (rest x)))))


;; Exercise 3.5 [h] (Exercise in altering structure.) Write a program that will play the
;; role of the guesser in the game Twenty Questions. The user of the program will have
;; in mind any type of thing. The program will ask questions of the user, which must
;; be answered yes or no, or "it" when the program has guessed it. If the program runs
;; out of guesses, it gives up and asks the user what "it" was. At first the program will
;; not play well, but each time it plays, it will remember the user's replies and use them
;; for subsequent guesses.

;;; ____________________________________________________________________________

(defstruct node
  name
  (yes nil)
  (no nil))

(defvar *db*
  (make-node :name 'animal
             :yes (make-node :name 'mammal)
             :no (make-node
                  :name 'vegetable
                  :no (make-node :name 'mineral))))

(defun give-up ()
  (format t "~&I give up - what is it? ")
  (make-node :name (read)))

(defun questions (&optional (node *db*))
  (format t "~&Is it a ~a? " (node-name node))
  (case (read)
    ((y yes) (if (not (null (node-yes node)))
                 (questions (node-yes node))
                 (setf (node-yes node) (give-up))))
    ((n no)  (if (not (null (node-no node)))
                 (questions (node-no node))
                 (setf (node-no node) (give-up))))
    (it 'aha!)
    (t (format t "Reply with YES, NO, or IT if I have guessed it.")
       (questions node))))

;;; ____________________________________________________________________________

;; Exercise 3.9 [m] Write a version of 'length' using the function `reduce'.

(defun length-r (list)
  (reduce #'+ (mapcar #'(lambda (x)
                          (declare (ignorable x))
                          1) list)))

(defun length-r (list)
  (reduce #'(lambda (x y)
              (declare (ignorable y))
              (+ x 1)) list
              :initial-value 0))

;; Test this one
(defun length-r (list)
  (reduce #'+ list :key #'(lambda (x) (declare-ignore x) 1)))

;;; TEST

(deftest test-length-r ()
  (check
    (= (length-r '(a b c)) 3)))
