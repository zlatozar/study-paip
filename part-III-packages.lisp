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
  (:export #:?-))

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
           #:clear-db
           #:clear-predicate
           #:unique-find-anywhere-if
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

(defpackage #:ch12-first
  (:documentation "Chapter 12. Compiling Logic Programs (first version)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :pat-base
                #:variable-p
                #:get-binding
                #:extend-bindings
                #:binding-val))

(defpackage #:ch12-second
  (:documentation "Chapter 12. Compiling Logic Programs (second version)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
    (:import-from :pat-base
                #:variable-p
                #:get-binding
                #:extend-bindings
                #:binding-val))

(defpackage #:ch12-final
  (:documentation "Chapter 12. Compiling Logic Programs (final version)")
  (:use #:common-lisp
        #:inspect
        #:tutor)
  (:import-from :paip-aux
                #:reuse-cons
                #:new-symbol
                #:length=1
                #:reuse-cons
                #:find-all
                #:find-if-anywhere
                #:find-anywhere)
  (:shadowing-import-from :paip-aux
                          #:symbol)
  (:import-from :pat-base
                #:fail
                #:no-bindings
                #:variable-p
                #:get-binding
                #:extend-bindings
                #:binding-val)
  (:export #:prolog-compile
           #:run-prolog
           #:top-level-prove
           #:show-prolog-vars/2
           ;; define build-in predicates
           #:top-level-query/0
           #:read/1
           #:write/1
           #:nl/0
           #:=/2
           #:==/2
           #:deref-equal
           #:call/1
           #:not/1
           #:bagof/3
           #:deref-copy
           #:setof/3
           #:is/2
           #:var/1
           #:lisp/2 ; note that there is no lisp/1
           #:repeat/0
           #:numberp/1
           #:atom/1

           #:!
           #:<-
           #:if
           #:and
           #:or
           ))

(defpackage #:ch12-exercises
  (:documentation "Selected exercises from Chapter 12")
  (:use #:common-lisp
        #:inspect
        #:tutor)

  )
