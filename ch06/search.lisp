;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH5-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; search.lisp: Search routines from section 6.4

(in-package #:ch6)

;;; ____________________________________________________________________________
;;;                                                                   Utilities

;; p. 192
;; To search in a tree you actually need... the tree. Simple way is to load given tree
;; and the second (clever) way is to load tree nodes when you need it (on every step) on
;; your algorithm. 'binary-tree' gives you next nodes you need it. Do not forget that
;; with second approach tree is infinite. You should pass to successors functions a piece
;; of data and this piece of data should contain relations. For example if you have a tree
;; (0 (1 (2) (3)) (4 (5 (6) NIL) NIL)) you should pass (1 (2) (3))) not just 1.

;; NOTE: Name 'binary-tree' is a confusing better could be 'gen-successors'. In practice
;; it returns next legal moves.

(defun binary-tree (x)
  (list (* 2 x) (+ 1 (* 2 x))))

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
with N nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(defun is (value)
  #'(lambda (x) (eql x value)))

;;; ____________________________________________________________________________

(defun prepend (x y)
  "Prepend Y to start of X"
  (append y x))

(defun diff (num)
  "Return the function that finds the difference from NUM."
  #'(lambda (x) (abs (- x num))))

;;; ____________________________________________________________________________
;;;                                                                 Tree search

;;; As a matter of style it is a good idea to name parameters that are functions
;;; with -fn suffix. For example: successors-fn, combiner-fn.

;; p.191
(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies GOAL-P. Start with STATES,
and search according to SUCCESSORS and COMBINER."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to COST-FN."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun price-is-right (price)
  "Return a function that measures the difference from PRICE,
but gives a big penalty for going over PRICE."
  #'(lambda (x) (if (> x price)
                    most-positive-fixnum
                    (- price x))))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
but never consider more than BEAM-WIDTH states at a time."
  (tree-search (list start) goal-p successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))

;;; ____________________________________________________________________________
;;;                                                                     Example

;; p.197
;; Consider the task of planning a flight across the North American continent in a small
;; airplane, one whose range is limited to 1000 kilometers. Suppose we have a list of
;; selected cities with airports, along with their position in longitude and latitude:

(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)
    (Chicago      87.37 41.50) (New-York       73.58 40.47)
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27)
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this NAME."
  (assoc name *cities*))

(defun trip (start dest)
  "Search for a way from the START to DEST."
  (beam-search start (is dest) #'neighbors
               #'(lambda (c) (air-distance c dest))
               1))

;;; ____________________________________________________________________________
;;;                                                          Search Paths p.200

;; To minimize the total distance, we need some way to talk about the path that leads
;; to the goal.

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

;; The next question is how to integrate paths into the searching routines with the least
;; amount of disruption. This suggests that we can use 'tree-search' unchanged if we pass
;; it paths instead of states, and give it functions that can process paths.

(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the START to DEST."
  (beam-search
   (make-path :state start)
   (is dest :key #'path-state)
   (path-saver #'neighbors #'air-distance
               #'(lambda (c) (air-distance c dest)))
   #'path-total-cost
   beam-width))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

;; New version of 'is' function
(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given VALUE."
  #'(lambda (path) (funcall test value (funcall key path))))

(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
         #'(lambda (new-state)
             (let ((old-cost
                    (+ (path-cost-so-far old-path)
                       (funcall cost-fn old-state new-state))))
               (make-path
                :state new-state
                :previous old-path
                :cost-so-far old-cost
                :total-cost (+ old-cost (funcall cost-left-fn
                                                 new-state)))))
         (funcall successors old-state)))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))

;; The notation #<...>, which is a Common Lisp convention for
;; printing output that can not be reconstructed by READ.

(defun show-city-path (path &optional (stream t))
  "Show the length of a PATH, and the cities along it."
  (format stream "#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
          (path-total-cost path)
          (reverse (map-path #'city-name path)))
  (values))

(defun map-path (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
            (map-path fn (path-previous path)))))

;;; ____________________________________________________________________________
;;;                         Guessing versus Guaranteeing a Good Solution p. 204

;; One way out of this dilemma is to start with a narrow beam width, and if that does
;; not lead to an acceptable solution, widen the beam and try again. We will call this
;; iterative widening.

(defun iter-wide-search (start goal-p successors cost-fn
                         &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
Return the first solution found at any width."
  (dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
        (iter-wide-search start goal-p successors cost-fn
                          :width (+ width 1) :max max))))

;;; ____________________________________________________________________________
;;;                                                     Searching graphs p. 206

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies GOAL-P. Start with STATES,
and search according to SUCCESSORS and COMBINER.
Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
            (funcall
             combiner
             (new-states states successors state= old-states)
             (rest states))
            goal-p successors combiner state=
            (adjoin (first states) old-states
                    :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)
           (member state old-states :test state=)))
   (funcall successors (first states))))

;; Used in examples p. 208
(defun next2 (x) (list (+ x 1) (+ x 2)))

;;; ____________________________________________________________________________
;;;                                                            A* search p. 209

;; The complication is in deciding which path to keep when two paths reach the same
;; state. If we have a cost function, then the answer is easy: keep the path with the
;; cheaper cost. Best-first search of a graph removing duplicate states is
;; called A*-search.

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                  &optional (state= #'eql) old-paths)
  "Find a path whose state satisfies GOAL-P. Start with PATHS,
and expand SUCCESSORS, exploring least cost first.
When there are duplicate states, keep the one with the
lower cost and discard the other."
  (dbg :search ";; Search: ~a" paths)
  (cond
    ((null paths) fail)
    ((funcall goal-p (path-state (first paths)))
     (values (first paths) paths))
    (t (let* ((path (pop paths))
              (state (path-state path)))
         ;; Update PATHS and OLD-PATHS to reflect
         ;; the new successors of STATE:
         (setf old-paths (insert-path path old-paths))
         (dolist (state2 (funcall successors state))
           (let* ((cost (+ (path-cost-so-far path)
                           (funcall cost-fn state state2)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-path
                          :state state2 :previous path
                          :cost-so-far cost
                          :total-cost (+ cost cost2)))
                  (old nil))
             ;; Place the new path, path2, in the right list:
             (cond
               ((setf old (find-path state2 paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path
                               path2 (delete old paths)))))
               ((setf old (find-path state2 old-paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (insert-path path2 paths))))))
         ;; Finally, call A* again with the updated path lists:
         (a*-search paths goal-p successors cost-fn cost-left-fn
                    state= old-paths)))))

;;; A*-helper functions

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is PATH1 cheaper than PATH2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put PATH into the right position, sorted by total cost."
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this PATH."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))

;;; ____________________________________________________________________________
;;;                                          Find all possible solutions p. 211

;; To find all solutions to a problem, all we have to do is pass in a goal predicate that
;; always fails, but saves all the solutions in a list. The goal predicate will see all
;; possible solutions and save away just the ones that are real solutions.

(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;; Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (beam-search
     start #'(lambda (x)
               (when (funcall goal-p x) (push x solutions))
               nil)
     successors cost-fn beam-width)
    solutions))
