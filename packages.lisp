;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; package.lisp: PAIP packages that are public book API

(in-package :cl-user)

;;; ____________________________________________________________________________
;;;                                                                      Public

(defpackage #:paip
  (:documentation "Expose functions and complete programs from PAIP book.")
  (:use #:common-lisp
        ;; tools
        #:inspect
        #:pcl-test
        ;; book code
        #:paip-aux
        #:ch1-exercises
        #:ch2-exercises
        #:ch3
        #:ch4-exercises
        #:pat-base
        #:ch5-exercises
        #:ch6
        #:ch6-exercises)

  ;; `inspect'
  (:export #:?a
           #:??
           #:?p~
           #:?p+
           #:?mac)

  ;; `pcl-test'
  (:export #:deftest
           #:check)

  ;; Avoid name collision with CL-USER:DEBUG
  (:shadowing-import-from :paip-aux
                          #:debug)
  ;; `paip-aux'
  (:export #:random-elt
           #:declare-ignore
           #:true
           #:false
           #:find-all
           #:find-all-if
           #:dbg
           #:dbg-indent
           #:enable-dbg
           #:disable-dbg
           #:starts-with
           #:member-equal
           #:flatten
           #:length=1)

  ;; `ch1-exercises'
  (:export #:power
           #:count-anywhere
           #:dot-product)

  ;; `ch2-exercises'
  (:export #:cross-product
           #:combine-all)

  ;; `ch3'
  (:export #:while-t)

  ;; `ch4-exercises'
  (:export #:permutations)

  ;; `pat-base'
  (:export #:fail)

  ;; Use optimized version of MAPPEND
  (:shadowing-import-from :ch5-exercises
                          #:mappend)

  ;; `ch5-exercises'
  (:export #:mappend)

  ;; `ch6'
  (:export #:pat-match
           #:pat-match-abbrev
           #:expand-pat-match-abbrev
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

  ;; `ch6-exercises'
  (:export #:compose))

;;; ____________________________________________________________________________
;;;                                                                        Done

(format *debug-io* "~2%Loading is done. Happy hacking!")
