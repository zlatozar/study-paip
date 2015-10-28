;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH9; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File exercises.lisp

(in-package #:ch9)

;;; ____________________________________________________________________________
;;;                                                              Implementation

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (declare (optimize (safety 0) (speed 3)))
  (declare (fixnum n test-divisor))
  (cond ((> (the fixnum (* test-divisor test-divisor)) n) n)
	((divides? test-divisor n) test-divisor)
	(t (find-divisor n (1+ test-divisor)))))

(defun divides? (a b)
  (declare (optimize (safety 0) (speed 3)))
  (declare (fixnum a b))
  (zerop (rem b a)))

(defun prime? (n)
  (= n (smallest-divisor n)))

(defun primes (a b)
  (loop for i from a to b when (prime? i)
       collect i))

;;; ____________________________________________________________________________
;;;                                                              Profiling/Test

(defvar *answers* (primes 1 1000))

(defun test-it (&optional (profile t))
  "Time a test run, check the results"
  (let ((answers (if profile
                     ;; Profile functions that you are interesting of
		     (with-profiling (divides? primes prime? find-divisor)
		       (primes 1 1000))
		     (time (primes 1 1000)))))
    (assert-equal answers *answers*)
    t))
