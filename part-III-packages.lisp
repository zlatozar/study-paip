;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; part-III-packages.lisp: PART III "Tools and techniques"

(in-package :cl-user)

;;; ____________________________________________________________________________

(defpackage #:ch9
  (:documentation "Chapter 9. Efficiency Issues")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:length=1
                #:memo
                #:memoize)
  (:export #:kwote
           #:assert-equal
           ;; Pipes
           #:make-pipe
           #:empty-pipe
           #:head
           #:tail
           #:elt-pipe
           #:enumerate-pipe
           #:filter-pipe
           #:map-pipe
           #:append-pipes
           #:mapcan-pipe
           #:combine-all-pipes
           ;; see exercises
           #:pipe-null-p
           #:reduce-pipe
           #:pipe->list
           ;; Profiling
           #:with-profiling))

;;; ____________________________________________________________________________

(defpackage #:ch10
  (:documentation "Chapter 10. Low-Level Efficiency Issues")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:reuse-cons)
  (:export #:make-trie
           #:put-trie
           #:get-trie
           #:delete-trie
           #:find-trie
           #:follow-arc))

(defpackage #:ch10-exercises
  (:documentation "Selected exercises from Chapter 10")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:reuse-cons))

(defpackage #:ch11
  (:documentation "Chapter 11. Logic Programming (unification)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:reuse-cons)
  (:import-from :pat-base
                #:fail
                #:no-bindings
                #:variable-p
                #:get-binding
                #:extend-bindings
                #:lookup)
  (:export #:unify
           #:unifier
           #:subst-bindings))

(defpackage #:ch11-first
  (:documentation "Chapter 11. Logic Programming (Prolog first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:ch11)
  (:import-from :pat-base
                #:fail
                #:no-bindings
                #:variable-p)
  (:import-from :ch6
                #:pat-match)
  (:export #:<-
           #:?-))

(defpackage #:ch11-final
  (:documentation "Chapter 11. Logic Programming (Prolog final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor
        #:ch11)
  (:import-from :paip-aux
                #:reuse-cons)
  (:import-from :pat-base
                #:fail
                #:no-bindings
                #:variable-p)
  (:export #:add-clause
           #:unique-find-anywhere-if
           #:<-
           #:?-))

(defpackage #:ch11-exercises
  (:documentation "Selected exercises from Chapter 11")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :ch11-final
                #:add-clause)
  (:export #:fact
           #:rule))
