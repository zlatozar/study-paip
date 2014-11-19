;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps-srch.lisp: Section 6.4 GPS based on explicit search

(in-package #:ch6)

;; In these solutions we search forward from the start to the goal; this is quite
;; different from the means-ends approach of searching backward from the goal for an
;; appropriate operator.

(defun search-gps (start goal &optional (beam-width 10))
  "Search for a sequence of operators leading to GOAL."
  (find-all-if
   #'action-p
   (beam-search
    (cons '(start) start)
    #'(lambda (state) (subsetp goal state :test #'equal))
    #'gps-successors
    #'(lambda (state)
        (+ (count-if #'action-p state)
           (count-if #'(lambda (con)
                         (not (member-equal con state)))
                     goal)))
    beam-width)))

(defun gps-successors (state)
  "Return a list of states reachable from this one using ops."
  (mapcar
   #'(lambda (op)
       (append
        (remove-if #'(lambda (x)
                       (member-equal x (op-del-list op)))
                   state)
        (op-add-list op)))
   (applicable-ops state)))

(defun applicable-ops (state)
  "Return a list of all ops that are applicable now."
  (find-all-if
   #'(lambda (op)
       (subsetp (op-preconds op) state :test #'equal))
   *ops*))
