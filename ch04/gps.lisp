;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH4-FINAL; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps.lisp: Final version of GPS

(in-package #:ch4-final)

;;; ____________________________________________________________________________
;;;                                                      From the first version

(defstruct op "An operation"
           (action nil) (preconds nil) (add-list nil) (del-list nil))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

;;; ____________________________________________________________________________

;; First main change:

;; Instead of printing a message when each operator is applied, we will instead have
;; GPS return the resulting state.

(defun executing-p (x)
  "Is X of the form: (executing ...) ?"
  (starts-with x 'executing))

;; also in `paip-aux'
(defun starts-with (list x)
  "Is this a LIST whose first element is X?"
  (and (consp list) (eql (first list) x)))

;; Each message is actually a condition, a list of the form - (executing operator)
;; It is good idea to have program return a meaningful value rather than print that value,
;; if there is the possibility that some other program might ever want to use the value.

(defun convert-op (op)
  "Make OP conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

;;; ____________________________________________________________________________

(mapc #'convert-op *school-ops*)

;;; ____________________________________________________________________________

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
           (action nil) (preconds nil) (add-list nil) (del-list nil))

;; Adding the (start) condition at the beginning also serves to differentiate between a
;; problem that cannot be solved and one that is solved without executing any
;; actions. Failure returns nil, while a solution with no steps will at least include the
;; (start) condition, if nothing else.

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from STATE, achieve GOALS using *OPS*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

;;; ____________________________________________________________________________

;; Second main change: The introduction of a goal stack to solve the recursive subgoal
;; problem.

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end.
Return true or current state."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

;; The program keeps track of the goals it is working on and immediately fails if a goal
;; appears as a subgoal of itself. This test is made in the second clause of 'achieve'.

(defun achieve (state goal goal-stack)
  "A GOAL is achieved if it already holds,
or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

;;; ____________________________________________________________________________

;; `apply-op' now returns the new state instead of printing anything. It first computes
;; the state that would result from achieving all the preconditions of the operator. If it
;; is possible to arrive at such a state, then `apply-op' returns a new state derived from
;; this state by adding what's in the add-list and removing everything in the delete-list.

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if OP is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))

      ;; States become ordered lists, because we need to preserve the ordering of
      ;; actions. Thus, we have to use the functions 'append' and 'remove-if', since these
      ;; are defined to preserve order.
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An OP is appropriate to a GOAL if it is in its add list."
  (member-equal goal (op-add-list op)))

;;; ____________________________________________________________________________

(defun use (oplist)
  "Use OPLIST as the default list of operators."
  ;; Return something useful, but not too verbose:
  ;; the number of operators.
  (length (setf *ops* oplist)))

;;; ____________________________________________________________________________

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

;;; ____________________________________________________________________________

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

;; e.g (9 14) (9 8) says that there is a path from 9 to 14 and 8
(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

;;; ____________________________________________________________________________

;; We said remove atoms, when we really meant to remove all conditions except the (START)
;; and (EXECUTING action) forms. Let's fix it.

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from STATE, achieve GOALS using *OPS*."
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is X something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

;;; ____________________________________________________________________________

;; Representation of the actions taken rather than just printing them out. The reason
;; this is an advantage is that we may want to use the results for something, rather than
;; just look at them. Suppose we wanted a function that gives us a path through a maze...

(defun find-path (start end)
  "Search a maze for a path from START to END."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

;;; ____________________________________________________________________________

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))        ; a from b to c
          (push (move-op a 'table b) ops)         ; a from table to b
          (push (move-op a b 'table) ops))))      ; a from b to table
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

;;; ____________________________________________________________________________

;; The "prerequisite clobbers sibling goal" situation is recognized, but the program
;; doesn't do anything about it. One thing we could do is try to vary the order (given and
;; reversed) of the conjunct goals. That is, we could change 'achieve-all' as follows:

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

;;; ____________________________________________________________________________

;; Operators with all preconditions filled would always be tried before other operators.

(defun achieve (state goal goal-stack)
  "A GOAL is achieved if it already holds,
or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state))))) ;***

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op)
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))
