;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; part-II-packages.lisp: PART II "Early AI Programs"

(in-package :cl-user)

;;; ____________________________________________________________________________

(defpackage #:ch4-first
  (:documentation "Chapter 4. GPS: The General Problem Solver (first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:find-all))


(defpackage #:ch4-final
  (:documentation "Chapter 4. GPS: The General Problem Solver (final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:mappend
                #:find-all
                #:find-all-if
                #:undebug
                #:dbg-indent)
  (:shadowing-import-from :paip-aux
                          #:debug)

  ;; Utilities (needed for Chapter 6)
  (:export #:op-action
           #:op-preconds
           #:op-add-list
           #:op-del-list
           #:*ops*
           #:use
           #:action-p
           #:make-block-ops
           #:move-op
           #:move-ons)
  ;; Final version of GPS
  (:export #:gps
           #:make-op
           #:find-path))

(defpackage #:ch4-exercises
  (:documentation "Selected exercises form Chapter 4")
  (:use #:common-lisp
        #:pcl-test)
  (:export #:permutations
           #:test-permutations))

;;; ____________________________________________________________________________

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
        #:pat-base)
  (:import-from :paip-aux
                #:flatten
                #:starts-with
                #:random-elt)
  (:export #:rule-pattern
           #:rule-responses
           #:switch-viewpoint))

(defpackage #:ch5-final
  (:documentation "Chapter 5. ELIZA: Dialog with a Machine (final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:pat-base
        #:ch5-first)
  (:import-from :paip-aux
                #:starts-with
                #:random-elt
                #:flatten
                #:mklist)
  (:export #:read-line-no-punct
           #:punctuation-p
           #:print-with-spaces
           #:eliza))

(defpackage #:ch5-exercises
  (:documentation "Selected exercises form Chapter 5")
  (:use #:common-lisp
        #:pcl-test)
  (:export #:mappend
           #:test-mappend))

;;; ____________________________________________________________________________

(defpackage #:ch6
  (:documentation "Chapter 6. Building Software Tools")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:pat-base)
  (:import-from :paip-aux
                #:dbg
                #:find-all-if
                #:undebug
                #:declare-ignore
                #:flatten
                #:random-elt)
  (:shadowing-import-from :paip-aux
                          #:debug)
  (:import-from :ch4-final
                #:op-action
                #:op-preconds
                #:op-add-list
                #:op-del-list
                #:*ops*
                #:use
                #:action-p
                #:make-block-ops
                #:move-op
                #:move-ons)
  (:import-from :ch5-first
                #:switch-viewpoint)

  (:export #:pat-match
           #:binary-tree
           #:finite-binary-tree
           #:prepend
           #:diff
           #:is
           #:deg->radians
           #:sorter
           #:distance
           #:tree-search
           #:depth-first-search
           #:breadth-first-search
           #:finite-binary-tree
           #:best-first-search
           #:beam-search
           #:iter-wide-search
           #:graph-search
           #:a*-search
           #:search-all
           #:rule-based-translator
           #:pat-match-abbrev
           #:expand-pat-match-abbrev))

(defpackage #:ch6-exercises
  (:documentation "Selected exercises form Chapter 6")
  (:use #:common-lisp
        #:pcl-test
        #:ch6)
  (:export #:compose
           #:search-n
           #:test-depth-first-search))

;;; ____________________________________________________________________________

(defpackage #:ch7
  (:documentation "Chapter 7. STUDENT: Solving Algebra Word Problems")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:shadow #:exp)
  (:import-from :pat-base
                #:binding-val
                #:binding-var)
  (:import-from :ch6
                #:pat-match
                #:rule-based-translator
                #:pat-match-abbrev
                #:expand-pat-match-abbrev))

(defpackage #:ch7-exercises
  (:documentation "Selected exercises form Chapter 7")
  (:use #:common-lisp
        #:pcl-test
        #:ch7))

;;; ____________________________________________________________________________

(defpackage #:ch8
  (:documentation "Chapter 8. Symbolic Mathematics: A Simplification Program")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:shadow #:exp)
  (:import-from :paip-aux
                #:starts-with
                #:length=1)
  (:export #:infix->prefix
           #:prefix->infix
           #:simplify
           #:simp))

(defpackage #:ch8-exercises
  (:documentation "Selected exercises form Chapter 8")
  (:use #:common-lisp
        #:pcl-test
        #:ch8))

;;; ____________________________________________________________________________

(defpackage #:ch9
  (:documentation "Chapter 9. Efficiency Issues")
  (:use #:common-lisp
        #:inspect)
  (:import-from :paip-aux
                #:length=1
                #:memoize)
  (:export #:kwote
           ;; Pipes
           #:make-pipe
           #:head
           #:tail
           #:elt-pipe
           #:enumerate-pipe
           #:filter-pipe
           #:map-pipe
           #:append-pipes
           #:mapcan-pipe
           #:combine-all-pipes
           ;; Profiling
           #:with-profiling
           ))
