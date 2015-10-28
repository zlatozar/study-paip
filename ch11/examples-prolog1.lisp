;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH11-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-prolog1.lisp

(in-package #:ch11-first)

(defexamples 11.1 "Logic Programming (first implementation)"
  ""
  "The idea behind logic programming is that the programmer should state the"
  "relationships that describe a problem and its solution."
  "In this chapter we develop an interpreter for the Prolog language."

  (:section "11.1 Idea 1: A Uniform Data Base")
  ""
  "First let's make sure we're dealing with a brand new database."

  ((clear-db))
  ""
  "Facts are entered into the data base with the <- macro"

  ((<- (likes Kim Robin)) @ 350)
  ((<- (likes Sandy Lee)))
  ((<- (likes Sandy Kim)))
  ((<- (likes Robin cats)))
  ""
  "We can also enter rules, which state contingent facts."

  ((<- (likes Sandy ?x) (likes ?x cats)) @ 351)
  ((<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))

  (:section "11.2 Idea 2: Unification of Logic Variables")
  ""
  ((pat-match '(?x + ?y) '(2 + 1)) :=> ((?y . 1) (?x . 2)) @ 352)
  ((unify '(?x + 1) '(2 + ?y)) :=> ((?y . 1) (?x . 2)))
  ((unify '(f ?x) '(f ?y)) :=> ((?x . ?y)))
  ((unify '(?a + ?a = 0) '(?x + ?y = ?y)) :=> ((?y . 0) (?x . ?y) (?a . ?x)))
  ((unifier '(?a + ?a = 0) '(?x + ?y = ?y)) :=> (0 + 0 = 0))
  ""
  "Let's try UNIFY on some (more) examples:"

  ((unify '(?x ?y a) '(?y ?x ?x)) :=> ((?y . a) (?x . ?y)) @ 357)
  ((unify '?x '(f ?x)) :=> nil)
  ((unify 'a 'a) :=> ((t . t)))
  ""
  "Here are some examples of UNIFIER:"

  ((unifier '(?x ?y a) '(?y ?x ?x)) :=> (a a a))
  ((unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
            '(?z + (4 * 5) + 3))
   :=> ((?a * 5 ^ 2) + (4 * 5) + 3))
  ""
  "Programming with Prolog"
  "First we define the MEMBER relation in Prolog:"

  ((<- (member ?item (?item . ?rest))) @ 358)
  ((<- (member ?item (?x . ?rest)) (member ?item ?rest)))
  ""
  "Now we can make some queries:"

  ((?- (member 2 (1 2 3))))
  ((?- (member 2 (1 2 3 2 1))))
  ((?- (member ?x (1 2 3))))
  ""
  "Let's add one more rule to the Sandy and the cats facts:"

  ((<- (likes ?x ?x)) @ 363)
  ""
  "Now we can ask some queries:"
  ((?- (likes Sandy ?who)) @ 365)
  ((?- (likes ?who Sandy)))
  ((?- (likes Robin Lee)))
  ((?- (likes ?x ?y) (likes ?y ?x)) @ 366)
)
