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
        #:ch11-final
        #:ch11-exercises
        #:ch12-final
        #:ch12-exercises)

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

  (:shadowing-import-from :ch12-final
                          #:?-)
  ;; `paip-aux'
  (:export ;; used in ch03
           #:find-all
           #:find-all-if
           #:declare-ignore
           #:true
           #:false
           ;; used in ch04
           #:enable-dbg
           #:disable-dbg
           #:dbg
           #:dbg-indent
           #:member-equal
           ;; used in ch05
           #:flatten      ;; see the version in `ch10' p. 329
           #:mklist
           #:random-elt
           #:starts-with
           ;; used in ch08
           #:length=1
           #:find-anywhere
           ;; used in ch09
           #:memoize
           #:delay
           #:force
           ;; used in ch10
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
           ;; used in ch11
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
           #:clear-db
           #:clear-predicate)

  ;; `ch11-exercises'
  (:export #:fact
           #:rule)

  ;; `ch12-final'
  (:export #:prolog-compile
           #:compile-body
           #:def-prolog-compiler-macro
           #:maybe-add-undo-bindings
           #:run-prolog
           #:top-level-prove
           #:deref
           #:deref-equal
           #:deref-copy
           #:show-prolog-vars/2
           #:top-level-query/0

           ;; Prolog build in predicate
           #:read/1    #:read
           #:write/1   #:write
           #:nl/0      #:nl
           #:=/2       #:=
           #:==/2      #:==
           #:call/1    #:call
           #:not/1     #:not
           #:bagof/3   #:bagof
           #:setof/3   #:setof
           #:is=/2     #:is=
           #:var/1     #:var
           #:repeat/0  #:repeat
           #:numberp/1 #:numberp
           #:atom/1    #:atom
           #:lisp/2    #:lisp

           #:?-
           #:<-
           #:!
           #:if
           #:nil)

  ;; `ch12-exercises'
  (:export #:prolog-trace

           #:and
           #:or

           #:true=/0    #:true=
           #:false=/0   #:false=
           #:>/2        #:>
           #:numberp=/1 #:numberp=)

  )


;;; ____________________________________________________________________________
;;;                                                                        Done

(format *debug-io* "~%Loading is done. Happy hacking!~%")
