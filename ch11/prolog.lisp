;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH11-FINAL; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

(in-package #:ch11-final)

;;;; Does not include destructive unification (11.6); see `prologc.lisp'

;; When we have key value, properties list fit perfectly. Value is just first rest.
;; Compare with parsing in `ch2::rule-rhs'.

;; Clauses are represented as (head . body) cons cells.
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; Clauses are stored on the predicate's plist.
;; Predicate is the relation name: (<- (likes Sandy cats)). 'likes' is the predicate.
(defun get-clauses (pred) (get pred 'clauses)) ; (<predicate name> clauses)
(defun predicate (relation) (first relation))

(defun args (x) "The arguments of a relation" (rest x))

;; Stores relations names for example for:
;;
;; (<- (likes Sandy ?x) (likes ?x cats)) => 'likes' will be stored in *db-predicate*

(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

;; Creates plist (clauses <all clauses as list>) and assign it to symbol.
;; Symbol name is the name of the predicate.

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate - relation
name. It must be a non-variable symbol."
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(defun rename-variables (x)
  "Replace all variables in X with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

;; Idea first was introduced in `ch4-final::achieve-all'
(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
          #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all
                  (append (clause-body new-clause) other-goals)
                  (unify goal (clause-head new-clause) bindings))))
          clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))

(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))

(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '?))))

(defun replace-?-vars (exp)
    "Replace any ? within EXP with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp))))
