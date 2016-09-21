;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH14-FIRST; Base: 10 -*-

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File krep1.lisp: Knowledge representation code; first version.

(in-package #:ch14-first)

;;; ____________________________________________________________________________

;; An `nlist' is implemented as a (count . elements) pair:
(defun make-empty-nlist ()
  "Create a new, empty nlist."
  (cons 0 nil))

(defun nlist-n (x) "The number of elements in an nlist." (car x))
(defun nlist-list (x) "The elements in an nlist." (cdr x))

(defun nlist-push (item nlist)
  "Add a new element to an NLIST."
  (incf (car nlist))
  (push item (cdr nlist))
  nlist)

;;; ____________________________________________________________________________

(defstruct (dtree (:type vector))
  (first nil) (rest nil) (atoms nil) (var (make-empty-nlist)))

;;; ____________________________________________________________________________

;; Not all Lisps handle the closure properly, so change the local PREDICATES
;; to a global *predicates*
(defvar *predicates* nil)

(defun get-dtree (predicate)
  "Fetch (or make) the `dtree' for this PREDICATE."
  (cond ((get predicate 'dtree))
    (t (push predicate *predicates*)
       (setf (get predicate 'dtree) (make-dtree)))))

(defun clear-dtrees ()
  "Remove all the dtrees for all the predicates."
  (dolist (predicate *predicates*)
    (setf (get predicate 'dtree) nil))
  (setf *predicates* nil))

;;; ____________________________________________________________________________

(defun index (key)
  "Store key in a `dtree' node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
  (dtree-index key key (get-dtree (predicate key))))

(defun dtree-index (key value dtree)
  "Index value under all atoms of KEY in dtree."
  (cond
    ((consp key)               ; index on both first and rest
     (dtree-index (first key) value
                  (or (dtree-first dtree)
                      (setf (dtree-first dtree) (make-dtree))))
     (dtree-index (rest key) value
                  (or (dtree-rest dtree)
                      (setf (dtree-rest dtree) (make-dtree)))))
    ((null key))               ; don't index on nil
    ((variable-p key)          ; index a variable
     (nlist-push value (dtree-var dtree)))
    (t ;; Make sure there is an nlist for this atom, and add to it
     (nlist-push value (lookup-atom key dtree)))))

(defun lookup-atom (atom dtree)
  "Return (or create) the nlist for this atom in dtree."
  (or (lookup atom (dtree-atoms dtree))
      (let ((new (make-empty-nlist)))
        (push (cons atom new) (dtree-atoms dtree))
        new)))

;;; ____________________________________________________________________________

(defun test-index ()
  (let ((props '((p a b) (p a c) (p a ?x) (p b c)
                 (p b (f c)) (p a (f . ?x)))))
    (clear-dtrees)
    (mapc #'index props)
    (write (list props (get-dtree 'p))
           :circle t :array t :pretty t)
    (values)))

;;; ____________________________________________________________________________

(defun fetch (query)
  "Return a list of buckets potentially matching the QUERY,
  which must be a relation of form (predicate . args)."
  (dtree-fetch query (get-dtree (predicate query))
               nil 0 nil most-positive-fixnum))

;;; ____________________________________________________________________________

(defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n)
  "Return two values: a list-of-lists of possible matches to pat,
  and the number of elements in the list-of-lists."
  (if (or (null dtree) (null pat) (variable-p pat))
      (values best-list best-n)
      (let* ((var-nlist (dtree-var dtree))
             (var-n (+ var-n-in (nlist-n var-nlist)))
             (var-list (if (null (nlist-list var-nlist))
                           var-list-in
                           (cons (nlist-list var-nlist)
                                 var-list-in))))
        (cond
          ((>= var-n best-n) (values best-list best-n))
          ((atom pat) (dtree-atom-fetch pat dtree var-list var-n
                                        best-list best-n))
          (t (multiple-value-bind (list1 n1)
                 (dtree-fetch (first pat) (dtree-first dtree)
                              var-list var-n best-list best-n)
               (dtree-fetch (rest pat) (dtree-rest dtree)
                            var-list var-n list1 n1)))))))

(defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n)
  "Return the answers indexed at this atom (along with the vars),
  or return the previous best answer, if it is better."
  (let ((atom-nlist (lookup atom (dtree-atoms dtree))))
    (cond
      ((or (null atom-nlist) (null (nlist-list atom-nlist)))
       (values var-list var-n))
      ((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n))
       (values (cons (nlist-list atom-nlist) var-list) var-n))
      (t (values best-list best-n)))))

;;; ____________________________________________________________________________

(proclaim '(inline mapc-retrieve))

(defun mapc-retrieve (fn query)
  "For every fact that matches the query,
  apply the function to the binding list."
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((bindings (unify query answer)))
        (unless (eq bindings fail)
          (funcall fn bindings))))))

;;; ____________________________________________________________________________

(defun retrieve (query)
  "Find all facts that match QUERY. Return a list of bindings."
  (let ((answers nil))
    (mapc-retrieve #'(lambda (bindings) (push bindings answers))
                   query)
    answers))

(defun retrieve-matches (query)
  "Find all facts that match QUERY.
  Return a list of expressions that match the query."
  (mapcar #'(lambda (bindings) (subst-bindings bindings query))
          (retrieve query)))

;;; ____________________________________________________________________________

(defmacro query-bind (variables query &body body)
  "Execute the body for each match to the QUERY.
  Within the BODY, bind each variable."
  (let* ((bindings (gensym "BINDINGS"))
         (vars-and-vals
           (mapcar
             #'(lambda (var)
                 (list var `(subst-bindings ,bindings ',var)))
             variables)))
    `(mapc-retrieve
       #'(lambda (,bindings)
           (let ,vars-and-vals
             ,@body))
       ,query)))
