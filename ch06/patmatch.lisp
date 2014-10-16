;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File patmatch.lisp

(in-package #:ch6)

;; For recurent patthers alternative is to create an abstraction, in the form of functions
;; and perhaps data structures, and refer explicitly to that abstraction in each new
;; application - in other words, to capture the abstraction in the form of a useable
;; software tool.
;;
;; Here is some examples:

;; p. 177
(defun compose (f g)
  "Return the function that computes (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

;; p. 178
;; This version also allows the prompt to be either a string or a function of no
;; arguments that will be called to print the prompt. See also ex. 6.3 p. 216

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
     (handler-case                 ; try-catch analog
         (progn
           (if (stringp prompt)
               (print prompt)
               (funcall prompt))
           (print (funcall transformer (read))))
       ;; In case of error, do this:
       (error (condition)
         (format t "~&;; Error ~a ignored, back to top level."
                 condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))

;;; ____________________________________________________________________________
;;;                                                             Pattern matcher

;; When the description of a problem gets this complicated, it is a good idea to
;; attempt a more formal specification:

;; pat         => var                  match any one expression
;;                constant             match just this atom
;;                segment-pat          match something against a sequence
;;                single-pat           match something against one expression
;;                (pat . pat)          match the first and the rest

;; single-pat  => (?is  var predicate) test predicate on one expression
;;                (?or  pat...)        match any pattern on one expression
;;                (?and pat...)        match every pattern on one expression
;;                (?not pat...)        succeed if pattern(s) do not match

;; segment-pat => ((?* var)  ...)      match zero or more expressions
;;                ((?+ var)  ...)      match one or more expressions
;;                ((?? var)  ...)      match zero or one expression
;;                ((?if exp) ...)      test if 'exp' (which may contain variables) is true

;; var         => ?chars               a symbol starting with ?
;; constant    => atom                 any nonvariable atom

;; Examples:

;; > (pat-match '(?x (?or < = >) ?y) '(3 < 4))
;; ((?Y . 4) (?X . 3))

;; > (pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3))
;; ((?N . 3))

;; p. 181
(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match PATTERN against INPUT in the context of the BINDINGS"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

;; From previous chapter
(defun variable-p (x)
  "Is X a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;; ____________________________________________________________________________

;; The table would say "if you see ?* in the pattern, then use the function
;; segment-match," and so on. This style of programming, where pattern/action pairs are
;; stored in a table, is called 'data-driven programming'. It is a very flexible style that
;; is appropriate for writing extensible systems.

;; Define two property lists (tables): key and action function
(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

;;; ____________________________________________________________________________

;; A function that looks up a data-driven function and calls it (such as 'segment-matcher'
;; and 'single-matcher') is called a dispatch function.

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

;;; Action taking functions

(defun segment-match-fn (x)
  "Get the segment-match function for X,
if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for X,
if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

;;; Individual matching functions for SINGLE-MATCH table

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the INPUT satisfies pred,
where VAR-AND-PRED is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the PATTERNS match the INPUT."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the PATTERNS match the INPUT."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns)
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the PATTERNS match the INPUT.
This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

;;; Individual matching functions for SEGMENT-MATCH table

;; Allow nonconstant patterns to follow segment variables.
(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against INPUT."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that PAT1 could possibly match INPUT,
starting at position START. If PAT1 is non-constant, then just
return START."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ; *** fix (in book p. 185 is <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of INPUT."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of INPUT."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

;; 'match-if' example:
;;
;; > (pat-match '(?x ?op ?y is ?z (?if (eql (funcall ?op ?x ?y) ?z))) '(3 + 4 is 7))
;; (?Z . 7) (?Y . 4) (?0P . +) (?X . 3))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
The PATTERN looks like ((?if code) . rest)."
  (and (progv
           (mapcar #'car bindings) ; ***
           (mapcar #'cdr bindings) ; ***
         (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

;; The idea is that a call to 'match-if' gets called like:

;; (match-if '((?if code) . rest) input ((v1 val1) (v2 val2) ...))

;; and 'eval' is called with `(second (first pattern))`, which the value of
;; `code`. However, 'eval' is called within the 'progv' that binds v1, v2, ..., to the
;; corresponding val1, val2, ..., so that if any of those variables appear free in code,
;; then they are bound when code is evaluated. Use (trace match-if) to see bindings.

;;; ____________________________________________________________________________

;; Second one is more readable right? p. 187
;; (a (?* ?x) (?* ?y) d)
;; (a ?x* ?y* d)

;; Many readers find the second pattern easier to understand at a glance. We could
;; change 'pat-match' to allow for patterns of the form ?x*, but that would mean
;; 'pat-match' would have a lot more work to do on every match. An alternative is
;; to leave 'pat-match' as is, but define another level of syntax for use by human readers
;; only. That is, a programmer could type the second expression above, and have it
;; translated into the first, which would then be processed by 'pat-match'.

(defun pat-match-abbrev (symbol expansion)
  "Define SYMBOL as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
        (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in PAT."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

;;; ____________________________________________________________________________
;;;                                         Convert 'use-eliza-rules' into tool

;; Here is what is needed (p. 188):

;; What kind of rule to use. Every rule will be characterized by an if-part and a
;; then-part, but the ways of getting at those two parts may vary.

;; What list of rules to use. In general, each application will have its own list of
;; rules.

;; How to see if a rule matches. By default, we will use 'pat-match', but it should
;; be possible to use other matchers.

;; What to do when a rule matches. Once we have determined which rule to use,
;; we have to determine what it means to use it. The default is just to substitute
;; the bindings of the match into the then-part of the rule.

;; See how can be used in eliza-pm.lisp
(defun rule-based-translator
    (input rules &key (matcher 'pat-match)
                   (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in RULES that matches INPUT,
and apply the action to that rule."
  (some
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule)
                              input)))
         (if (not (eq result fail))
             (funcall action result (funcall rule-then rule)))))
   rules))
