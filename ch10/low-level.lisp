;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH10; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;; File low-level.lisp: Low level optimization

(in-package #:ch10)

(defun sum-squares (seq)
  (let ((sum 0))
    (dotimes (i (length seq))
      (incf sum (square (elt seq i))))
    sum))

(defun square (x)
  (* x x))

;; NOTE: In HyperSpec for `simple-array' definition is used word 'dimension' which is a
;;       bit misleading when you read (simple-array fixnum *). It is equivalent to
;;       (simple-array (*)). Actually the length of the list is the array dimension,
;;       values are the dimensions length. When it is a star * it means unknown length.

(defun sum-squares (vect)
  (declare (type (simple-array fixnum *) vect) ; *** an array with indeterminate length
	   (inline square)
	   (optimize (speed 3) (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length seq))
      (declare (fixnum i))
      (incf sum (the fixnum (square (svref vect i))))) ; *** `the' is a special operator!
    sum))

(defmacro defun* (fn-name arg-list &body body)
  "Define two functions, one an interface to a &keyword-less version.
Proclaim the interface function inline."
  (if (and (member '&key arg-list) (not (member '&rest arg-list)))
      (let ((no-key-fn-name (symbol fn-name '*no-key))
	    (args (mapcar #'first-or-self (set-difference arg-list lambda-list-keywords))))
	`(progn
	  (proclaim '(inline ,fn-name))
	  (defun ,no-key-fn-name ,args
	    ,@body)
	  (defun ,fn-name ,arg-list
	    (,no-key-fn-name ,@args))))
      `(defun ,fn-name ,arg-list
	 ,@body)))
