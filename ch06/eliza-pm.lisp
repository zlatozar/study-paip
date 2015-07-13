;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH6; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza-pm.lisp: Updated version of eliza in section 6.3

(in-package #:ch6)

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
     (print 'eliza>)
     (print (flatten (use-eliza-rules (read))))))

;; Using defined tools in patmatch.lisp now we could use it for ELIZA.
(defun use-eliza-rules (input)
  "Find some rule with which to transform the INPUT."
  (rule-based-translator input *eliza-rules*
                         :action #'(lambda (bindings responses)
                                     (sublis (switch-viewpoint bindings)
                                             (random-elt responses)))))
