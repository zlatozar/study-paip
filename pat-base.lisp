;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PAT-BASE; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;;; File pat-base.lisp: Contain functions that are used during 'pat-match' creation.
;;;;                     It is used in Chapters 5 and 6.

(in-package #:pat-base)

(defconstant fail nil "Indicates pat-match failure")

(defvar no-bindings '((t . t))
  "Indicates `pat-match' success, with no variables")

;; There is no 'variable type' in Common Lisp. There is now way to distinguish them like
;; in Prolog where they begin with capital letter. Let's use symbols with some name
;; convention then. In particular, symbols have names, which are strings and are
;; accessible through the 'symbol-name' function. Strings in turn have elements that are
;; characters, accessible through the function char. The character '?' is denoted by the
;; self-evaluating escape sequence #\?.

(defun variable-p (x)
  "Is X a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  "Get the value part (for VAR) from a binding list."
  (binding-val (get-binding var bindings)))

(defun match-variable (var input bindings)
  "Does VAR match INPUT? Uses (or updates) and returns BINDINGS."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

;; Following function is a good example of conditional consing/adding. It show also how to
;; use list-consing recursion. 'pat-match' has as a parameter 'bindings' - it is CONS
;; parameter. As each recursive call returns, we (possibly) add to this CONS parameter.

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy 'no-bindings' (aka (T . T))
        (if (eq bindings no-bindings)
            nil
            bindings)))

;;; ____________________________________________________________________________

;; If both pattern and input are lists, we first call 'pat-match' recursively on the first
;; element of each list. This returns a binding list (or 'fail'), which we use to match
;; the rest of the lists.

;; Basic version p.158
(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match PATTERN against INPUT in the context of the BINDINGS"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))     ; ***
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))
