;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; package.lisp: Define PAIP packages. Every chapter is a package or
;;;; set of packages.

(in-package :cl-user)

;;; ____________________________________
;;;                               Tools

(defpackage #:inspect
  (:documentation "This package is not part of the book.
It contains functions that are collected from other places and
could help during coding sessions.")
  (:use #:common-lisp)
  (:export #:??
           #:?a
           #:?p~
           #:?p+
           #:?p*
           #:?p%))

(defpackage #:pcl-test
  (:documentation "Use defined in the book 'Practical Common Lisp' test framework
to test chapter exercises.")
  (:use #:common-lisp)
  (:export #:deftest
           #:check))

;;; ____________________________________
;;;               Book helper functions

(defpackage #:paip-aux
  (:documentation "Useful functions defined in the book.")
  (:use #:common-lisp)
  (:shadow #:symbol
           #:debug)

  (:export #:starts-with
           #:mappend
           #:dbg-indent
           #:find-all
           #:find-all-if
           #:declare-ignore
           #:debug
           #:undebug
           #:dbg-indent))

;;; ____________________________________
;;;                        Run examples

(defpackage #:tutor
  (:documentation "Use defined in the book test framework
and run chapter examples. If in chapter there is more than one version
only final is included.")
  (:use #:common-lisp
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug)

  (:export #:defexamples
           #:do-examples
           #:do-chapter))

;;; ____________________________________
;;;            Book chapters with tools

;;; PART I "Introduction to Common Lisp"

(defpackage #:ch1
  (:documentation "Chapter 1: Introduction to Lisp")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch1-exercises
  (:documentation "Selected exercises form Chapter 1")
  (:use #:common-lisp
        #:ch1
        #:pcl-test))

(defpackage #:ch2
  (:documentation "Chapter 2: A Simple Lisp Program")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch2-exercises
  (:documentation "Selected exercises form Chapter 2")
  (:use #:common-lisp
        #:ch2
        #:pcl-test))

(defpackage #:ch3
  (:documentation "Chapter 3: Overview of Lisp")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch3-exercises
  (:documentation "Selected exercises form Chapter 3")
  (:use #:common-lisp
        #:ch3
        #:paip-aux
        #:pcl-test)
  (:shadowing-import-from #:common-lisp :debug))


;;; PART II "Early AI Programs"


(defpackage #:ch4-first
  (:documentation "Chapter 4: GPS: The General Problem Solver (first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch4-final
  (:documentation "Chapter 4: GPS: The General Problem Solver (final version)")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:paip-aux :debug)
  ;; Expose final version
  (:export #:gps))

;;; ____________________________________
;;;                             Exposed

(defpackage #:paip
  (:documentation "Expose functions from PAIP book that could be
used in projects.")
  (:use #:common-lisp
        #:ch4-final))
