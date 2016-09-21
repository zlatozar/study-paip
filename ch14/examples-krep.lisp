;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH14-FINAL; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples-krep.lisp

(in-package #:ch14-final)

(defexamples 14 "Knowledge Representation and Reasoning - final version"
  ""
  (:section "14.10 Solutions to the Expressiveness Problems")
  ""
  "In this section we introduce a frame-like language, using the primitives"
  "sub, rel, ind, val, and and."
  ""

  "We add some facts about dogs and bears, both as individuals and species:"
  ""
  ((add-fact '(sub dog animal)) @ 488)
  ((add-fact '(sub bear animal)))
  ((add-fact '(ind Fido dog)))
  ((add-fact '(ind Yogi bear)))
  ((add-fact '(val color Yogi brown)))
  ((add-fact '(val color Fido golden)))
  ((add-fact '(val latin-name bear ursidae)))
  ((add-fact '(val latin-name dog canis-familiaris)))

  ;; "Now retrieve-fact is used to answer three questions: What kinds of animals"
  ;; "are there?"
  ;; ""
  ;; ((retrieve-fact '(sub ?kind animal)) =>
  ;;  (((?KIND . DOG))
  ;;   ((?KIND . BEAR))))

  ;; "What are the Latin names of each kind of animal?"
  ;; ""
  ;; ((retrieve-fact '(and (sub ?kind animal)
  ;;                   (val latin-name ?kind ?latin))) =>
  ;;  (((?LATIN . CANIS-FAMILIARIS) (?KIND . DOG))
  ;;   ((?LATIN . URSIDAE) (?KIND . BEAR))))

  ;; "What are the colors of each individual bear?"
  ;; ""
  ;; ((retrieve-fact '(and (ind ?x bear) (val color ?x ?c))) @ 489 =>
  ;;  (((?C . BROWN) (?X . YOGI))))
  ;; ((test-bears) @ 492)
  )
