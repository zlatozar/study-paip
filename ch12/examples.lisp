;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH12-FINAL; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch12-final)

(defexamples 12 "Compiling Logic Programs"
  ""
  "This chapter presents a compiler that translates from Prolog to Lisp."
  "Unfortunatley, there's not much to see in terms of examples."
  "But we load the files for you, in case you want to play with them."
  ""
  ((prolog-compile 'likes) @ 389)
  ((prolog-compile 'member))      ; defined as build-in predicate

  ;; Do not uncomment because it requires user input
  ;;
  ;; ((?- (= ?p member) (call (?p ?x (a b c)))) @ 415)
  ;; ((?- (member ?x (a b c)) (not (= ?x b))) @ 416)
  ;; ((?- (not (= ?x b)) (member ?x (a b c))) @ 416)
)
