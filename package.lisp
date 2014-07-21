;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; package.lisp: Define PAIP packages. Every chapter is a package.

(in-package :cl-user)

;;; ____________________________________
;;;                               Tools

(defpackage #:tools
  (:documentation "This package is not part of the book.
It contains functions that are collected from other places and
could help during coding sessions.")
  (:use #:common-lisp)
  (:export #:??
           #:?a
           #:?f
           #:?s
           #:?t
           #:?v))

;;; ____________________________________
;;;               Book helper functions

(defpackage #:paip-aux
  (:documentation "Useful functions defined in the book.")
  (:use #:common-lisp)
  (:shadow #:symbol
           #:debug)

  (:export #:starts-with
           #:mappend))

;;; ____________________________________
;;;            Book chapters with tools

;;; PART I

(defpackage #:ch1
  (:documentation "Chapter 1: Introduction to Lisp")
  (:use #:common-lisp
        #:tools
        #:paip-aux)

  (:export #:first-name
           #:last-name))

(defpackage #:ch2
  (:documentation "Chapter 2: A Simple Lisp Program")
  (:use #:common-lisp
        #:tools
        #:paip-aux)

  (:export #:*grammar*
           #:*bigger-grammar*

           #:sentence
           #:noun-phrase
           #:verb-phrase
           #:generate
           #:generate-tree))

(defpackage #:ch3
  (:documentation "Chapter 3: Overview of Lisp")
  (:use #:common-lisp
        #:tools
        #:paip-aux)

  (:export #:length9
           #:tree-equal
           #:same-shape-tree
           #:adder
           #:bank-account))

;;; PART II

;;; ____________________________________
;;;                        Run examples

(defpackage #:tutor
  (:documentation "Use defined in the book test framework
and run chapter examples.")
  (:use #:common-lisp
        #:paip-aux

        #:ch1
        #:ch2
        #:ch3)

  (:export #:do-examples
           #:do-chapter))

;;; ____________________________________
;;;                             Exposed

(defpackage #:paip
  (:documentation "Expose functions from PAIP books that could be
used in projects.")
  (:use #:common-lisp))
