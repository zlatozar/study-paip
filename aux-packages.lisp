;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; aux-packages.lisp: Packages that contain utilities and auxiliary functions

(in-package :cl-user)

;;; ____________________________________________________________________________
;;;                                                Tools (not part of the book)

(defpackage #:inspect
  (:documentation "This package is not part of the book.
It contains functions that could help during coding sessions.")
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
  (:nicknames #:test)
  (:use #:common-lisp)
  (:export #:deftest
           #:check))

;;; ____________________________________________________________________________
;;;                                              Helper functions from the book

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
           #:enable-dbg  ; alias of 'DEBUG'
           #:undebug
           #:disable-dbg ; alias of 'UNDEBUG'
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
           ;; ch09
           #:memoize
           ))

;;; ____________________________________________________________________________
;;;                                                                Run examples

(defpackage #:tutor
  (:documentation "Use defined in the book test framework
and run chapter examples.")
  (:use #:common-lisp)
  (:import-from :paip-aux
                #:starts-with)
  (:export #:defexamples
           #:do-examples
           #:do-chapter))

(format *debug-io* "~2&
To run all examples:
    (tutor:do-examples :all)

To run examples from particular chapter:
    (tutor:do-examples <chapter number>)")
