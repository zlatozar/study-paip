;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH3-EXERCISES; Base: 10 -*-

;;;; File binary-trees.lisp

(in-package #:ch3-exercises)

;;                 1
;;               / | \
;;              2  3  4
;;             / \   /
;;            6  7  9
;;                 /
;;                12

;; How to represent a tree in Common Lisp?
;;
;; The easiest way is as nested list.
;; For example: (1 (2 (6 7)) (3) (4 (9 (12))))
;;
;; The first element of a list is the root of that tree (or a node) and the rest of the
;; elements are the subtrees (every new open parenthesis is a next level of the tree).
;; For example, the subtree (6 7) is a tree with 2 at its root and the elements 6 7
;; as its children. So I start digging on this.
;;
;; NOTE: Another representation is to use Paul Graham "ANSI Common Lisp"
;; representation:

(defstruct tree-node
  elt (l nil) (r nil))

;; If we have binary tree may be a better way to represent it with nested lists is to
;; use a-list (more readable). For example:
;; (1 (2 (6 . 7)) (4 (9 (12))))
;;
;; Even better - display empty:
;; (0 (1 (2 . 3) NIL) (4 (5 (6) NIL) NIL))
;;
;; What is a node? (elm (left right))
;;
;; Hmm, last representation is not consistent - (left right) could not be treated as 'elm'
;; e.g. list. Let's fix it: (0 (1 (2) (3)) (4 (5 (6))))
;;
;; Great now I have the tree! For tests let's use:

(defparameter *bin-tree* '(0 (1 (2) (3)) (4 (5 (6)))))

;;; Define needed function

(defun make-bin-leaf (elm)
  "Create a leaf."
  (list elm))

(defun make-bin-node (parent elm1 &optional elm2)
  (list parent elm1 elm2))

(defun node-elm (node)
  (first node))

(defun node-left (node)
  (first (second node)))

(defun node-right (node)
  (first (third node)))

;;; Predicates

(defun leaf-p (node)
  "Test if binary tree NODE is a leaf."
  (and (listp node)
       (= (list-length node) 1)))

(defun node-p (node)
  "Test if binary tree NODE is a node."
  (and (listp node)
       (= (list-length node) 3)))

;; Later in book, `paip-aux:find-anywhere' will be defined
(defun member-p (elm tree)
  (eql (find-anywhere elm tree) elm))

;;; ____________________________________________________________________________
;;;                                                                Simple tests

(deftest test-make-bin-tree ()
  (check
    (equal (make-bin-node 0
                          (make-bin-node 1 (make-bin-leaf 2)
                                           (make-bin-leaf 3))
                          (make-bin-node 4 (make-bin-node 5
                                                          (make-bin-leaf 6))))
           '(0 (1 (2) (3)) (4 (5 (6) NIL) NIL)) )))

(deftest test-find-anywhere ()
  (check
    (eql (find-anywhere 3 *bin-tree*) 3)))
