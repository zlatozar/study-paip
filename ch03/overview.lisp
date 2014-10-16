;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH3; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File overview.lisp: miscellaneous functions from Chapter 3

(in-package #:ch3)

(defun tax-bracket (income)
  "Determine what percent tax should be paid for this INCOME."
  (cond ((< income 10000.00) 0.00)
        ((< income 30000.00) 0.20)
        ((< income 50000.00) 0.25)
        ((< income 70000.00) 0.30)
        (t                   0.35)))

;;; ____________________________________________________________________________

;; p. 57
(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "Increment the wins for the player with highest score."
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

;;; ____________________________________________________________________________
;;;                                                     Various LENGTH versions

(defun length1 (list)
  (let ((len 0))               ; start with LEN=0
    (dolist (element list)     ; and on each iteration
      (declare (ignorable element))
      (incf len))              ; increment LEN by 1
    len))                      ; and return LEN

(defun length1.1 (list)        ; alternate version:
  (let ((len 0))               ; (not my preference)
    (dolist (element list len) ; uses len as result here
      (declare (ignorable element))
      (incf len))))

(defun length2 (list)
  (let ((len 0))               ; start with LEN=0
    (mapc #'(lambda (element)  ; and on each iteration
              (declare (ignorable element))
              (incf len))      ;  increment LEN by 1
          list)
    len))                      ; and return LEN

(defun length3 (list)
  (do ((len 0 (+ len 1))       ; start with LEN=0, increment
       (l list (rest l)))      ; ... on each iteration
      ((null l) len)))         ; (until the end of the list)

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

(defun true (x)
  (declare (ignorable x))
  t)

(defun length7 (list)
  (count-if #'true list))

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))


(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

;;; ____________________________________________________________________________
;;;                                                              Tail recursive

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun length10 (list)
  (length10-aux list 0))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

;;; ____________________________________________________________________________

;; p. 65
(defun product (numbers)
  "Multiply all the NUMBERS together to compute their product."
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (RETURN 0)     ;; style: return to be in upper case
          (setf prod (* n prod))))))

;;; ____________________________________________________________________________
;;;                                                        WHILE implementation

;; p. 67
(defmacro while (test &rest body)
  "Repeat BODY while TEST is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

(defmacro while (test &rest body)
  "Repeat BODY while TEST is true."
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

(defmacro while (test &rest body)
  "Repeat BODY while TEST is true."
  `(loop (unless ,test (return nil))
      ,@body))

;;; ____________________________________________________________________________

(defun true (&rest ignore)
  (declare (ignorable ignore))
  t)

;; p. 76
(defun same-shape-tree (a b)
  "Are two trees the same except for the leaves?"
  (tree-equal a b :test #'true))

(defun english->french (words)
  (sublis '((are . va) (book . libre) (friend . ami)
            (hello . bonjour) (how . comment) (my . mon)
            (red . rouge) (you . tu))
          words))

;;; ____________________________________________________________________________

;; p. 87
(defun average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined.")
      (/ (reduce #'+ numbers)
         (length numbers))))

(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))

;;; ____________________________________________________________________________

;; p. 88
(defun sqr (x)
  "Multiply X by itself."
  (check-type x number)
  (* x x))

(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x))
  (* x x))

(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x) (x))
  (* x x))

;;; ____________________________________________________________________________

;; In pseudo code:
;;
;; (defun eat-porridge (bear)
;;   (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
;;           (bear (bear-porridge bear))
;;           "~a's porridge is not just right: ~a"
;;           bear (hotness (bear-porridge bear)))
;;   (eat (bear-porridge bear)))

;;; ____________________________________________________________________________
;;;                                                                    Closures

;; p. 92
(defun adder (c)
  "Return a function that adds C to its argument."
  #'(lambda (x) (+ x c)))

(defun bank-account (balance)
  "Open a bank account starting with the given BALANCE."
  #'(lambda (action amount)
      (case action
        (deposit  (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

;;; ____________________________________________________________________________

;; p. 97
(defun math-quiz (op range n)
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))

;;; ____________________________________________________________________________

(defun math-quiz (&optional (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

;;; ____________________________________________________________________________

;; NOTE: The function 'find-all' described in book is defined in ../auxfns.lisp

;;; ____________________________________________________________________________

;; p. 102
(defmacro while (test &body body)
  "Repeat BODY while TEST is true."
  `(loop (if (not ,test) (return nil))
      . ,body))

;; STYLE: &aux can be used to bind a new local variable or variables, as if bound with
;; let*. Because &aux variables are not parameters at all and thus have no place in a
;; parameter list they should be clearly distinguished as local variables with a let.

(defun length14 (list &aux (len 0))
  (dolist (element list len)
    (declare (ignorable element))
    (incf len)))
