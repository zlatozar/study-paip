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
        #:ch4-final
        #:ch4-exercises
        #:pat-base
        #:ch5-final
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
  (:export ;; ch03
           #:find-all
           #:find-all-if
           #:declare-ignore
           #:true
           #:false
           ;; ch04
           #:enable-dbg
           #:disable-dbg
           #:dbg
           #:dbg-indent
           #:member-equal
           ;; ch05
           #:flatten
           #:mklist
           #:random-elt
           #:starts-with
           ;; ch08
           #:length=1
           #:find-anywhere
           )

  ;; `ch1-exercises'
  (:export #:power
           #:count-anywhere
           #:dot-product)

  ;; `ch2-exercises'
  (:export #:cross-product
           #:combine-all)

  ;; `ch3'
  (:export #:while)

  ;; `ch4-final'
  (:export #:make-op
           #:gps
           #:find-path)

  ;; `ch4-exercises'
  (:export #:permutations)

  ;; `pat-base'
  (:export #:fail)

  ;; Use optimized version of `paip-aux:mappend'
  (:shadowing-import-from :ch5-exercises
                          #:mappend)

  ;; `ch05-final'
  (:export #:read-line-no-punct
           #:punctuation-p
           #:print-with-spaces)

  ;; `ch5-exercises'
  (:export #:mappend)

  ;; `ch6'
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
           #:search-gps
           #:rule-based-translator
           #:pat-match-abbrev
           #:expand-pat-match-abbrev)

  ;; `ch6-exercises'
  (:export #:compose))

;;; ____________________________________________________________________________
;;;                                                                        Done

(format *debug-io* "~2%Loading is done. Happy hacking!")
