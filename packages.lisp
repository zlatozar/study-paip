;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; package.lisp: Define PAIP packages. Every chapter is a package or
;;;;               set of packages.

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
           #:?p%
           #:?mac))

(defpackage #:pcl-test
  (:documentation "Use defined in the book 'Practical Common Lisp' test framework
to test chapter exercises.")
  (:use #:common-lisp)
  (:export #:deftest
           #:check))

;;; ____________________________________
;;;      Helper functions from the book

(defpackage #:paip-aux
  (:documentation "Useful functions defined in the book.")
  (:use #:common-lisp)
  (:shadow #:symbol
           #:debug)

  (:export #:mappend
           ;; ch03
           #:find-all
           #:find-all-if
           #:declare-ignore
           ;; ch04
           #:debug
           #:undebug
           #:dbg
           #:dbg-indent
           ;; ch05
           #:flatten
           #:mklist
           #:random-elt
           #:starts-with
           #:no-bindings
           #:fail
           #:match-variable))

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

(format *debug-io* "~2&
To run all examples:
    (tutor:do-examples :all)

To run examples from particular chapter:
    (tutor:do-examples <chapter number>)")

;;; ____________________________________
;;;            Book chapters with tools

;;; PART I "Introduction to Common Lisp"

(defpackage #:ch1
  (:documentation "Chapter 1. Introduction to Lisp")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch1-exercises
  (:documentation "Selected exercises form Chapter 1")
  (:use #:common-lisp
        #:pcl-test
        #:ch1)
  (:export #:power
           #:test-power
           #:count-atoms
           #:test-count-atoms
           #:count-all-atoms
           #:count-anywhere
           #:test-count-anywhere
           #:dot-product
           #:test-dot-product))

(defpackage #:ch2
  (:documentation "Chapter 2. A Simple Lisp Program")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch2-exercises
  (:documentation "Selected exercises form Chapter 2")
  (:use #:common-lisp
        #:pcl-test
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:cross-product
           #:combine-all
           #:test-combine-all))

(defpackage #:ch3
  (:documentation "Chapter 3. Overview of Lisp")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:while
           #:while2))

(defpackage #:ch3-exercises
  (:documentation "Selected exercises form Chapter 3")
  (:use #:common-lisp
        #:paip-aux
        #:pcl-test
        #:ch3)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:dprint
           #:pr-rest
           #:questions
           #:length-r
           #:test-length-r))


;;; PART II "Early AI Programs"


(defpackage #:ch4-first
  (:documentation "Chapter 4. GPS: The General Problem Solver (first version)")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch4-final
  (:documentation "Chapter 4. GPS: The General Problem Solver (final version)")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:paip-aux :debug)
  ;; Final version of GPS
  (:export #:gps)
  ;; Utilities (needed for Chapter 6)
  (:export #:make-block-ops
           #:use
           #:action-p
           #:*ops*
           #:op-preconds))

(format *debug-io* "~2%To run GPS:
    (ch4-final:gps <state> <goals> &optional <available operators>)")

(defpackage #:ch4-exercises
  (:documentation "Selected exercises form Chapter 4")
  (:use #:common-lisp
        #:pcl-test)
  (:export #:permutations
           #:test-permutations))

(defpackage #:ch5-first
  (:documentation "Chapter 5. ELIZA: Dialog with a Machine (first version)")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:rule-pattern
           #:rule-responses
           #:switch-viewpoint))

(defpackage #:ch5-final
  (:documentation "Chapter 5. ELIZA: Dialog with a Machine (final version)")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor
        #:ch5-first)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:eliza))

(format *debug-io* "~2%To run ELIZA:
    (ch5-final:eliza)
and type bye to exit")

(defpackage #:ch5-exercises
  (:documentation "Selected exercises form Chapter 5")
  (:use #:common-lisp
        #:pcl-test)
  (:export #:mappend
           #:test-mappend))

(defpackage #:ch6
  (:documentation "Chapter 6. Building Software Tools")
  (:use #:common-lisp
        #:inspect
        #:paip-aux
        #:tutor
        #:ch4-final
        #:ch5-final)
  (:shadowing-import-from #:paip-aux :debug)
  )

(defpackage #:ch6-exercises
  (:documentation "Selected exercises form Chapter 6")
  (:use #:common-lisp
        #:pcl-test)
  )

;;; ____________________________________
;;;                             Exposed

(defpackage #:paip
  (:documentation "Expose functions from PAIP book that could be
used in projects.")
  (:use #:common-lisp
        #:ch4-final
        #:ch5-final))

;;; ____________________________________
;;;                                Help

(format *debug-io* "~2%Loading is done. Happy hacking.")
