;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH10; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;; File trie.lisp: trie data structure implementation

(in-package #:ch10)

(defstruct trie value arcs)
;; To ease reload use `defvar' instead of (defconstant trie-deleted "deleted")
(defvar trie-deleted "deleted")

(defun put-trie (key trie value)
  "Set the value of key in trie"
  (setf (trie-value (find-trie key t trie)) value))

(defun get-trie (key trie)
  (let* ((key-trie (find-trie key nil trie))
	 (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val trie-deleted))
	(values nil nil)
	(values val t))))

(defun delete-trie (key trie)
  "Remove a key from a trie"
  (put-trie key trie trie-deleted))

(defun find-trie (key extend? trie)
  "Find the trie node for key.
If EXTEND? is true, make a new node if necessary"
  (cond ((null trie) nil)
	((atom key)
	 (follow-arc key extend? trie))
	(t (find-trie (cdr key)
		      extend?
		      (find-trie (car key)
				 extend?
				 (find-trie "."
					    extend?
					    trie))))))

(defun follow-arc (component extend? trie)
  "Find the trie node for this component of the key.
If EXTEND? is true, make a new node if necessary."
  (let ((arc (assoc component (trie-arcs trie))))
    (cond ((not (null arc)) (cdr arc))
	  ((not extend?) nil)
	  (t (let ((new-trie (make-trie)))
	       (push (cons component new-trie)
		     (trie-arcs trie))
	       new-trie)))))
