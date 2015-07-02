;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; part-I-packages.lisp: PART I "Introduction to Common Lisp"

(in-package :cl-user)

;;; ____________________________________________________________________________

(defpackage #:ch1
  (:documentation "Chapter 1. Introduction to Lisp")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:mappend))

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

;;; ____________________________________________________________________________

(defpackage #:ch2
  (:documentation "Chapter 2. A Simple Lisp Program")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:mappend))

(defpackage #:ch2-exercises
  (:documentation "Selected exercises form Chapter 2")
  (:use #:common-lisp
        #:pcl-test)
  (:import-from :paip-aux
                #:mappend)
  (:export #:cross-product
           #:test-cross-product
           #:combine-all
           #:test-combine-all))

;;; ____________________________________________________________________________

(defpackage #:ch3
  (:documentation "Chapter 3. Overview of Lisp")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:export #:while))

(defpackage #:ch3-exercises
  (:documentation "Selected exercises form Chapter 3")
  (:use #:common-lisp
        #:pcl-test
        #:ch3)
  (:import-from :paip-aux
                #:declare-ignore
                #:find-anywhere)
  (:export #:dprint
           #:questions
           #:length-r
           #:test-length-r))
