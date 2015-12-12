;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; package.lisp: PAIP packages that are public book API

(in-package :cl-user)

;;; ____________________________________________________________________________
;;;                                                                      Public

(defpackage #:paip
  (:documentation "Expose functions and complete programs from PAIP book.")
  (:nicknames #:norvig #:pn)
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
        #:ch6-exercises
        #:ch7
        #:ch8
        #:ch9
        #:ch10
        #:ch11
        #:ch11-final)

  ;; `inspect'
  (:export #:??
           #:?p
           #:?p%
           #:?p*
           #:!!
           #:?mac)

  ;; `pcl-test'
  (:export #:deftest
           #:check)

  ;; Avoid name collision with `cl:debug'
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
           #:flatten      ;; see the version in `ch10' p. 329
           #:mklist
           #:random-elt
           #:starts-with
           ;; ch08
           #:length=1
           #:find-anywhere
           ;; ch09
           #:memoize
           #:delay
           #:force
           ;; ch10
           #:reuse-cons
           #:defresource
           #:with-resource
           #:queue-contents
           #:make-queue
           #:enqueue
           #:dequeue
           #:front
           #:empty-queue-p
           #:queue-nconc
           ;; ch11
           #:unique-find-if-anywhere
           #:find-if-anywhere
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
           #:use
           #:gps
           #:find-path)

  ;; `ch4-exercises'
  (:export #:permutations)

  ;; `pat-base'
  (:export #:fail)

  ;; `ch05-final'
  (:export #:read-line-no-punct
           #:punctuation-p
           #:print-with-spaces)

  ;; Use optimized version of `paip-aux:mappend'
  (:shadowing-import-from :ch5-exercises
                          #:mappend)

  ;; `ch5-exercises'
  (:export #:mappend)

  ;; `ch6'
  (:export #:pat-match
           #:binary-tree
           #:finite-binary-tree
           #:prompt-generator
           #:prepend
           #:diff
           #:is
           #:deg->radians
           #:sorter
           #:distance
           #:tree-search
           #:depth-first-search
           #:breadth-first-search
           #:best-first-search
           #:beam-search
           #:iter-wide-search
           #:graph-search
           #:a*-search
           #:search-all
           #:pat-match-abbrev
           #:expand-pat-match-abbrev)

  ;; `ch6-exercises'
  (:export #:compose
           #:search-n)

  ;; `ch7'

  ;; `ch8'
  (:export #:infix->prefix
           #:prefix->infix
           #:simplify
           #:simp)

  ;; `ch9'
  (:export #:kwote
           #:assert-equal
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
           #:with-profiling)

  ;; `ch10'
  (:export #:make-trie
           #:put-trie
           #:get-trie
           #:delete-trie)

  ;; `ch11'
  (:export #:unify
           #:unifier)

  ;; `ch11-final'
  (:export #:unique-find-anywhere-if
           ;; Prolog user interface
           #:<-
           #:?-)

  )


;;; ____________________________________________________________________________
;;;                                                                        Done

(format *debug-io* "~%Loading is done. Happy hacking!~%")
