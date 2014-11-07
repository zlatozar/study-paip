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
           #:true
           #:false
           ;; ch04
           #:debug
           #:debugit
           #:undebug
           #:undebugit
           #:dbg
           #:dbg-indent
           #:member-equal
           ;; ch05
           #:flatten
           #:mklist
           #:random-elt
           #:starts-with))

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
        #:tutor
        #:paip-aux)
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
        #:tutor
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch2-exercises
  (:documentation "Selected exercises form Chapter 2")
  (:use #:common-lisp
        #:pcl-test
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:cross-product
           #:test-cross-product
           #:combine-all
           #:test-combine-all))

(defpackage #:ch3
  (:documentation "Chapter 3. Overview of Lisp")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:while))

(defpackage #:ch3-exercises
  (:documentation "Selected exercises form Chapter 3")
  (:use #:common-lisp
        #:pcl-test
        #:paip-aux
        #:ch3)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:dprint
           #:questions
           #:length-r
           #:test-length-r))


;;; PART II "Early AI Programs"


(defpackage #:ch4-first
  (:documentation "Chapter 4. GPS: The General Problem Solver (first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux)
  (:shadowing-import-from #:common-lisp :debug))

(defpackage #:ch4-final
  (:documentation "Chapter 4. GPS: The General Problem Solver (final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux)
  (:shadowing-import-from #:paip-aux :debug)
  ;; Final version of GPS
  (:export #:gps)
  ;; Utilities (needed for Chapter 6)
  (:export #:make-block-ops
           #:use
           #:action-p
           #:*ops*
           #:op-preconds))

(defpackage #:ch4-exercises
  (:documentation "Selected exercises form Chapter 4")
  (:use #:common-lisp
        #:pcl-test)
  (:export #:permutations
           #:test-permutations))

(defpackage #:pat-base
  (:documentation "Simple version of 'pat-match' and common functions.
In Chapters 5 and 6 'pat-match' will be improved.")
  (:use #:common-lisp)
  (:export #:fail
           #:no-bindings
           #:variable-p
           #:get-binding
           #:binding-val
           #:binding-var
           #:make-binding
           #:lookup
           #:match-variable
           #:extend-bindings))

(defpackage #:ch5-first
  (:documentation "Chapter 5. ELIZA: Dialog with a Machine (first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux
        #:pat-base)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:rule-pattern
           #:rule-responses
           #:switch-viewpoint))

(defpackage #:ch5-final
  (:documentation "Chapter 5. ELIZA: Dialog with a Machine (final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:paip-aux
        #:pat-base
        #:ch5-first)
  (:shadowing-import-from #:common-lisp :debug)
  (:export #:*eliza-rules*
           #:eliza))

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
        #:tutor
        #:paip-aux
        #:pat-base)
  (:shadowing-import-from #:paip-aux :debug)
  (:import-from :ch4-final
                #:make-block-ops
                #:use
                #:action-p
                #:*ops*
                #:op-preconds)
  (:import-from :ch5-final :*eliza-rules*)
  (:export #:pat-match
           #:binary-tree
           #:finite-binary-tree
           #:prepend
           #:diff
           #:is
           #:deg->radians
           #:tree-search
           #:depth-first-search
           #:breadth-first-search
           #:finite-binary-tree
           #:best-first-search
           #:beam-search
           #:iter-wide-search
           #:graph-search
           #:a*-search
           #:search-gps))

(defpackage #:ch6-exercises
  (:documentation "Selected exercises form Chapter 6")
  (:use #:common-lisp
        #:pcl-test
        #:ch6)
  (:export #:compose
           #:test-depth-first-search))

;;; ____________________________________
;;;                              Public

(defpackage #:paip
  (:documentation "Expose functions and complete programs from PAIP book.")
  (:use #:common-lisp
        #:inspect
        #:pcl-test)
  (:import-from :paip-aux
                #:random-elt
                #:declare-ignore
                #:true
                #:false
                #:find-all
                #:find-all-if
                #:dbg
                #:dbg-indent
                #:debugit
                #:undebugit
                #:starts-with
                #:member-equal
                #:flatten)
  (:import-from :ch1-exercises
                #:power
                #:count-anywhere
                #:dot-product)
  (:import-from :ch2-exercises
                #:cross-product
                #:combine-all)
  (:import-from :ch3
                #:while)
  (:import-from :ch4-exercises
                #:permutations)
  (:import-from :pat-base
                #:fail)
  (:import-from :ch5-exercises
                #:mappend)
  (:import-from :ch6
                #:pat-match
                #:binary-tree
                #:finite-binary-tree
                #:prepend
                #:diff
                #:is
                #:deg->radians
                #:tree-search
                #:depth-first-search
                #:breadth-first-search
                #:finite-binary-tree
                #:best-first-search
                #:beam-search
                #:iter-wide-search
                #:graph-search
                #:a*-search)
  (:import-from :ch6-exercises
                #:compose))

;;; ____________________________________
;;;                                Help

(format *debug-io* "~2%Loading is done. Happy hacking!")
