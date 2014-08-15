;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File overview.lisp: miscellaneous functions from Chapter 3

(in-package #:ch3)

(defun tax-bracket (income)
  "Determine what percent tax should be paid for this income."
  (cond ((< income 10000.00) 0.00)
        ((< income 30000.00) 0.20)
        ((< income 50000.00) 0.25)
        ((< income 70000.00) 0.30)
        (t                   0.35)))

;;; ____________________________________________________________________________

(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "Increment the WINS for the player with highest score."
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

;;; ____________________________________________________________________________

(defun length1 (list)
  (let ((len 0))               ; start with LEN=0
    (dolist (element list)     ; and on each iteration
      (declare (ignorable element))
      (incf len))              ; increment LEN by 1
    len))                      ; and return LEN

;;; ____________________________________________________________________________

(defun length1.1 (list)        ; alternate version:
  (let ((len 0))               ; (not my preference)
    (dolist (element list len) ; uses len as result here
      (declare (ignorable element))
      (incf len))))

;;; ____________________________________________________________________________

(defun length2 (list)
  (let ((len 0))               ; start with LEN=0
    (mapc #'(lambda (element)  ; and on each iteration
              (declare (ignorable element))
              (incf len))      ;  increment LEN by 1
          list)
    len))                      ; and return LEN

;;; ____________________________________________________________________________

(defun length3 (list)
  (do ((len 0 (+ len 1))       ; start with LEN=0, increment
       (l list (rest l)))      ; ... on each iteration
      ((null l) len)))         ; (until the end of the list)

;;; ____________________________________________________________________________

(defun length4 (list)
  (loop for element in list    ; go through each element
     count t))                 ;   counting each one

(defun length5 (list)
  (loop for element in list    ; go through each element
     summing 1))               ;   adding 1 each time

(defun length6 (list)
  (loop with len = 0           ; start with LEN=0
     until (null list)         ; and (until end of list)
     for element = (pop list)  ; on each iteration
     do (incf len)             ;  increment LEN by 1
     finally (return len)))    ; and return LEN

;;; ____________________________________________________________________________

(defun true (x)
  (declare (ignorable x))
  t)

(defun length7 (list)
  (count-if #'true list))


;;; ____________________________________________________________________________

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

;;; ____________________________________________________________________________

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

;;; ____________________________________________________________________________

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun length10 (list)
  (length10-aux list 0))

;;; ____________________________________________________________________________

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

;;; ____________________________________________________________________________

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

;;; ____________________________________________________________________________

(defun product (numbers)
  "Multiply all the numbers together to compute their product."
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (RETURN 0)
          (setf prod (* n prod))))))

;;; ____________________________________________________________________________

(defmacro while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

;;; ____________________________________________________________________________

(defmacro while (test &rest body)
  "Repeat body while test is true."
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

;;; ____________________________________________________________________________

(defmacro while (test &rest body)
  "Repeat body while test is true."
  `(loop (unless ,test (return nil))
      ,@body))

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

;;; ____________________________________________________________________________

(defun pr-rest (x)
  (cond ((null x))
        ((atom x) (princ " . ") (princ x))
        (t (princ " ") (dprint (first x)) (pr-rest (rest x)))))

;;; ____________________________________________________________________________

(defun true (&rest ignore)
  (declare (ignorable ignore))
  t)

(defun same-shape-tree (a b)
  "Are two trees the same except for the leaves?"
  (tree-equal a b :test #'true))

;;; ____________________________________________________________________________

(defun english->french (words)
  (sublis '((are . va) (book . libre) (friend . ami)
            (hello . bonjour) (how . comment) (my . mon)
            (red . rouge) (you . tu))
          words))

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

(defun average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined.")
      (/ (reduce #'+ numbers)
         (length numbers))))

;;; ____________________________________________________________________________

(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))

;;; ____________________________________________________________________________

(defun sqr (x)
  "Multiply x by itself."
  (check-type x number)
  (* x x))

;;; ____________________________________________________________________________

(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x))
  (* x x))

;;; ____________________________________________________________________________

(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x) (x))
  (* x x))

;;; ____________________________________________________________________________

;; (defun eat-porridge (bear)
;;   (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
;;           (bear (bear-porridge bear))
;;           "~a's porridge is not just right: ~a"
;;           bear (hotness (bear-porridge bear)))
;;   (eat (bear-porridge bear)))

;;; ____________________________________________________________________________

(defun adder (c)
  "Return a function that adds c to its argument."
  #'(lambda (x) (+ x c)))

;;; ____________________________________________________________________________

(defun bank-account (balance)
  "Open a bank account starting with the given balance."
  #'(lambda (action amount)
      (case action
        (deposit  (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

;;; ____________________________________________________________________________

(defun problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))

(defun math-quiz (op range n)
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

;;; ____________________________________________________________________________

(defun math-quiz (&optional (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

;;; ____________________________________________________________________________

(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

;;; ____________________________________________________________________________

;; NOTE: The function 'find-all' described in book is defined in ../auxfns.lisp

;;; ____________________________________________________________________________

(defmacro while2 (test &body body)
  "Repeat body while test is true."
  `(loop (if (not ,test) (return nil))
      . ,body))

;;; ____________________________________________________________________________

(defun length14 (list &aux (len 0))
  (dolist (element list len)
    (declare (ignorable element))
    (incf len)))

;;; ____________________________________________________________________________
;;;                                                                 See ex. 3.9

(defun length-r (list)
  (reduce #'+ (mapcar #'(lambda (x)
                          (declare (ignorable x))
                          1) list)))

(defun length-r (list)
  (reduce #'(lambda (x y)
              (declare (ignorable y))
              (+ x 1)) list
              :initial-value 0))

(defun length-r (list)
  (reduce #'+ list :key #'(lambda (x)
                            (declare (ignorable x))
                            1)))
