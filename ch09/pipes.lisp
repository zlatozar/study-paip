;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;;; File pipes.lisp: Building infinite sets

(in-package #:ch9)

(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe)
  (first pipe))

(defun tail (pipe)
  (let ((rp (rest pipe)))
    (if (functionp rp)
        (setf rp (funcall rp))
        rp)))

;;; ____________________________________________________________________________
;;;                                                  Utility functions on pipes

(defun elt-pipe (pipe i)
  (if (zerop i)
      (head pipe)
      (elt-pipe (tail pipe) (1- i))))

(defun enumerate-pipe (pipe &key count key (result pipe))
  "Go through all (or COUNT) elements in PIPE, applying KEY to each of them"
  (if (or (eq pipe empty-pipe) (eql count 0))
      result
      (progn
        (unless (null key)
          (funcall key (head pipe)))
        (enumerate-pipe
         (tail pipe) :count (if count (1- count)) :key key :result result))))

(defun filter-pipe (pred pipe)
  "Keep only items in (non-null) PIPE satisfying predicate"
  (if (eq pipe empty-pipe)
    empty-pipe
    (if (funcall pred (head pipe))
      (make-pipe (head pipe) (filter-pipe pred (tail pipe)))
      (filter-pipe pred (tail pipe)))))

(defun map-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
                 (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  (if (eq x empty-pipe)
      y
      (make-pipes (head x) (append-pipes (tail x) y))))

(defun mapcan-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      (let ((x (funcall fn (head pipe))))
        (make-pipe (head x)
                   (append-pipes (tail x)
                                 (mapcan-pipe fn (tail pipe)))))))

(defun combine-all-pipes (xp yp)
  (mapcan-pipe #'(lambda (y)
                   (map-pipe #'(lambda (x)
                                 (append-pipes x y))
                             xp))
               yp))

;;; ____________________________________________________________________________
;;;                                                                    Examples

;; How to create pipes example
(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-pipe start (integers (1+ start) end))
      nil))

;; Example
(defun sieve (pipe)
  (make-pipe (head pipe)
             (filter-pipe #'(lambda (x)
                         (/= (mod x (head pipe)) 0))
                     (sieve (tail pipe)))))

(defvar *primes* (sieve (integers 2)))

;; See `ch2::generate-all'
(defun generate-all (phrase)
  (if (listp phrase)
      (if (null phrase)
          (list nil)
          (combine-all-pipes (generate-all (first phrase))
                             (generate-all (rest phrase))))
      (let ((choices (rules-rhs (assoc phrase *grammar*))))
        (if choices
            (mapcan-pipe #'generate-all choices)
            (list (list phrase))))))
