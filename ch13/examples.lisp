;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH13; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File examples.lisp

(in-package #:ch13)

(defexamples 13 "Object Oriented Programming"
  ""
  "It is only natural that a wide range of programming styles have been"
  "introduced to attack the wide range of problems in this book."
  "One style not yet covered is 'object-oriented programming'."
  ""
  "Peter Wegner (1987) proposes the following formula as a definition:"
  "Object-orientation = Objects + Classes + Inheritance"

  (:section "13.2 Objects")
  ""
  "Now we're ready to get started."
  ""
  ((setf acct (new-account "J. Random Customer" 1000.00)) @ 438)
  ((send acct 'withdraw 500.00) => 500.0)
  ((send acct 'deposit 123.45) => 623.45)
  ((send acct 'name) => "J. Random Customer")
  ((send acct 'balance) => 623.45)

  (:section "13.4 Classes")
  ""
  "Now we define the class ACCOUNT with the define-class macro."
  ""
  ((define-class account (name &optional (balance 0.00))
     ((interest-rate .06))
     (withdraw (amt) (if (<= amt balance)
                         (decf balance amt)
                         'insufficient-funds))
     (deposit  (amt) (incf balance amt))
     (balance  ()    balance)
     (name     ()    name)
     (interest ()    (incf balance (* interest-rate balance)))) @ 440)
  ""
  "Here are the generic functions defined by this macro:"
  ((setf acct2 (account "A. User" 2000.00)))
  ((deposit acct2 42.00) => 2042.0)
  ((interest acct2) => 2164.52)
  ((balance acct2) => 2164.52 @ 441)
  ((balance acct) => 623.45)

  (:section "13.5 Delegation")
  ""
  "ATTENTION: This is not working because of using 'otherwise'"
  ""
  ;; ((define-class password-account (password acct) ()
  ;;                (change-password (pass new-pass)
  ;;                                 (if (equal pass password)
  ;;                                     (setf password new-pass)
  ;;                                     'wrong-password))
  ;;                (otherwise (pass &rest args)
  ;;                           (if (equal pass password)
  ;;                               (apply message acct args)
  ;;                               'wrong-password))))
  ;; ""
  ;; "Now we see how the class PASSWORD-ACCOUNT can be used to provide protection"
  ;; "for an existing account:"
  ;; ""
  ;; ((setf acct3 (password-account "secret" acct2)) @ 441)
  ;; ((balance acct3 "secret") => 2164.52)
  ;; ((withdraw acct3 "guess" 2000.00) => WRONG-PASSWORD)
  ;; ((withdraw acct3 "secret" 2000.00) => 164.52)

  (:section "13.7 CLOS: The Common Lisp Object System")
  ""
  "ATTENTION: Because some Lisp implementations can't convert a structure class into"
  "a CLOS class, nor convert a regular function into a generic function,"
  "we use the names account*, name*, balance*, interest-rate*.  If you were"
  "doing a REAL APPLICATION, not just some examples, you would choose one"
  "implementation and get to use the regular names."
  ""
  ((defclass account* ()
     ((name :initarg :name :reader name*)
      (balance :initarg :balance :initform 0.00 :accessor balance*)
      (interest-rate :allocation :class :initform .06
                     :reader interest-rate*))) @ 445)

  ((setf a1 (make-instance 'account* :balance 5000.00
                           :name "Fred")) @ 446)
  ((name* a1) => "Fred")
  ((balance* a1) => 5000.0)
  ((interest-rate* a1) => 0.06)

  ((defmethod withdraw* ((acct account*) amt)
     (if (< amt (balance* acct))
         (decf (balance* acct) amt)
         'insufficient-funds)) @ 446)

  ((defclass limited-account (account*)
     ((limit :initarg :limit :reader limit))))

  ((defmethod withdraw* ((acct limited-account) amt)
     (if (> amt (limit acct))
         'over-limit
         (call-next-method))))

  ((setf a2 (make-instance 'limited-account
                           :name "A. Thrifty Spender"
                           :balance 500.00 :limit 100.00)) @ 447)

  ((name* a2) => "A. Thrifty Spender")
  ((withdraw* a2 200.00) => OVER-LIMIT)
  ((withdraw* a2 20.00) => 480.0)

  (:section "13.8 A CLOS Example: Searching Tools")
  ""
  ((defclass problem ()
     ((states :initarg :states :accessor problem-states))) @ 449)

  ((defmethod searcher ((prob problem))
     "Find a state that solves the search problem."
     (cond ((no-states-p prob) fail)
           ((goal-p prob) (current-state prob))
           (t (let ((current (pop-state prob)))
                (setf (problem-states prob)
                      (problem-combiner
                       prob
                       (problem-successors prob current)
                       (problem-states prob))))
              (searcher prob)))))

  ((defmethod current-state ((prob problem))
     "The current state is the first of the possible states."
     (first (problem-states prob))))

  ((defmethod pop-state ((prob problem))
     "Remove and return the current state."
     (pop (problem-states prob))))

  ((defmethod no-states-p ((prob problem))
     "Are there any more unexplored states?"
     (null (problem-states prob))))

  ((defmethod searcher :before ((prob problem))
              (dbg 'search "~&;; Search: ~a" (problem-states prob))) @ 450)

  ((defclass eql-problem (problem)
     ((goal :initarg :goal :reader problem-goal))))

  ((defmethod goal-p ((prob eql-problem))
     (eql (current-state prob) (problem-goal prob))))

  ((defclass dfs-problem (problem) ()
     (:documentation "Depth-first search problem.")))

  ((defclass bfs-problem (problem) ()
     (:documentation "Breadth-first search problem.")))

  ((defmethod problem-combiner ((prob dfs-problem) new old)
     "Depth-first search looks at new states first."
     (append new old)))

  ((defmethod problem-combiner ((prob bfs-problem) new old)
     "Depth-first search looks at old states first."
     (append old new)))

  ((defclass binary-tree-problem (problem) ()) @ 451)

  ((defmethod problem-successors ((prob binary-tree-problem) state)
     (let ((n (* 2 state)))
       (list n (+ n 1)))))

  ((defclass binary-tree-eql-bfs-problem
       (binary-tree-problem eql-problem bfs-problem) ()))

  ((setf p1 (make-instance 'binary-tree-eql-bfs-problem
                           :states '(1) :goal 12)))
  ((searcher p1) => 12)

  ((defclass best-problem (problem) ()
     (:documentation "A Best-first search problem.")) @ 452)

  ((defmethod problem-combiner ((prob best-problem) new old)
     "Best-first search sorts new and old according to cost-fn."
     (sort (append new old) #'<
           :key #'(lambda (state) (cost-fn prob state)))))

  ((defmethod cost-fn ((prob eql-problem) state)
     (abs (- state (problem-goal prob)))))

  ((defclass beam-problem (problem)
     ((beam-width :initarg :beam-width :initform nil
                  :reader problem-beam-width))))

  ((defmethod problem-combiner :around ((prob beam-problem) new old)
              (let ((combined (call-next-method)))
                (subseq combined 0 (min (problem-beam-width prob)
                                        (length combined))))))

  ((defclass binary-tree-eql-best-beam-problem
       (binary-tree-problem eql-problem best-problem beam-problem)
     ()))

  ((setf p3 (make-instance 'binary-tree-eql-best-beam-problem
                           :states '(1) :goal 12 :beam-width 3)))

  ((searcher p3) => 12)

  ((defclass trip-problem (binary-tree-eql-best-beam-problem)
     ((beam-width :initform 1))) @ 453)

  ((defmethod cost-fn ((prob trip-problem) city)
     (air-distance (problem-goal prob) city)))

  ((defmethod problem-successors ((prob trip-problem) city)
     (neighbors city)))

  ((setf p4 (make-instance 'trip-problem
                           :states (list (city 'new-york))
                           :goal (city 'san-francisco))))

  ((searcher p4) =>
   (SAN-FRANCISCO 122.26 37.47))

  (:section "13.9 Is CLOS Object-oriented?")
  ""
  ((defmethod conc ((x null) y) y) @ 454)

  ((defmethod conc (x (y null)) x))

  ((defmethod conc ((x list) (y list))
     (cons (first x) (conc (rest x) y))))

  ((defmethod conc ((x vector) (y vector))
     (let ((vect (make-array (+ (length x) (length y)))))
       (replace vect x)
       (replace vect y :start1 (length x)))))

  ((conc nil '(a b c)) => (A B C) @ 455)
  ((conc '(a b c) nil) => (A B C))
  ((conc '(a b c) '(d e f)) => (A B C D E F))
  ((conc '#(a b c) '#(d e f)) => #(A B C D E F))
)
