;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PAIP-AUX; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;;;; File auxfns.lisp: Auxiliary functions used by all other programs

(in-package #:paip-aux)

(proclaim '(inline mappend random-elt starts-with member-equal
            mklist flatten compose last1 length=1
            rest2 rest3 symbol old-symbol reuse-cons
            queue-contents make-queue enqueue dequeue
            front empty-queue-p queue-nconc))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 1

;; p. 19. See also ex. 5.14
(defun mappend (fn list)
  "Append the results of calling FN on each element of LIST.
Like `mapcon', but uses `append' instead of `nconc'."
  (apply #'append (mapcar fn list)))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 2

;; p. 36
(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 3

(defun declare-ignore (&rest args)
  "Ignore the arguments."
  (declare (ignore args))
  nil)

;; p. 60
(defun true (&rest args)
  "Always return true."
  (declare (ignore args)) t)

(defun false (&rest args)
  "Always return false."
  (declare (ignore args)) nil)

;; How to create function alias p. 100
(setf (symbol-function 'find-all-if) #'remove-if-not)

;; p. 101
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of SEQUENCE that match ITEM,
according to the keywords. Doesn't alter SEQUENCE."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 4

;;; The Debugging Output Facility p. 124

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;; Shadows DEBUG from Common Lisp package
(defun debug (&rest ids)
  "Start `dbg' output on the given IDS."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop `dbg' on the IDS. With no IDS, stop `dbg' altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;; Functions alias (to avoid name clash) that are exposed in `paip' package
(setf (symbol-function 'enable-dbg) #'debug)
(setf (symbol-function 'disable-dbg) #'undebug)

;; p. 126
(defun starts-with (list x)
  "Is this a list whose first element is X?"
  (and (consp list) (eql (first list) x)))

;; p. 129
(defun member-equal (item list)
  (member item list :test #'equal))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 5

;; p. 165
(defun mklist (x)
  "If X is a list return it, otherwise return the list of X"
  (if (listp x)
      x
      (list x)))

(defun flatten (the-list)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist the-list))

;;; Pattern Matching Facility p. 155 - see `pat-base' package

;;; ____________________________________________________________________________
;;;                                                                   Chapter 6

;; More efficient version is on p. 217 (see ex. 6.5)
(defun compose (&rest functions)
  "Return the function that is the composition of all the args.
i.e. (compose f g h) = (lambda (x) (f (g (h x))))."
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 8

(defun length=1 (x)
  "Is X a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun find-anywhere (item tree)
  "Does ITEM occur anywhere in TREE?
Returns searched element if found else nil."
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

;;; ____________________________________________________________________________
;;;                                                                   Chapter 9

;;; The Memoization Facility (p. 270)

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;; What `memoize' does is fetch the original function and transform it with memo to a
;; function that, when called, will first look in the table to see if the answer is
;; already known. If not, the original function is called, and a new value is placed in
;; the table.

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace FN-NAME's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of FN."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

;;; ____________________________________________________________________________
;;;                                                                      Macros

(defmacro once-only (variables &rest body)
  "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
  (assert (every #'symbolp variables))
  (let ((temps nil))
    (dotimes (i (length variables)) (push (gensym) temps))
    `(if (every #'side-effect-free? (list .,variables))
         (progn .,body)
         (list 'let
               ,`(list ,@(mapcar #'(lambda (tmp var)
                                     `(list ',tmp ,var))
                                 temps variables))
               (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
                             variables temps)
                 .,body)))))

(defun side-effect-free? (exp)
  "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
  (or (atom exp) (constantp exp)
      (starts-with exp 'function)
      (and (starts-with exp 'the)
           (side-effect-free? (third exp)))))

(defmacro funcall-if (fn arg)
  (once-only (fn)
             `(if ,fn (funcall ,fn ,arg) ,arg)))

(defmacro read-time-case (first-case &rest other-cases)
  "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
  (declare (ignore other-cases))
  first-case)

;;; ____________________________________________________________________________

(defun rest2 (x)
  "The rest of a list after the first two elements."
  (rest (rest x)))

(defun rest3 (list)
  "The rest of a LIST after the first three elements."
  (cdddr list))

(defun partition-if (pred list)
  "Return 2 values: elements of LIST that satisfy PRED,
and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
true if EXPS is nil, EXPS if there is only one,
and (and exp1 exp2...) if there are several EXPS."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ____________________________________________________________________________

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
         (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))

(defun maybe-set-fill-pointer (array new-length)
  "If this is an ARRAY with a fill pointer, set it to
NEW-LENGTH, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array)
            (max (fill-pointer array) new-length))))

;;; ____________________________________________________________________________

;;; NOTE:
;;;
;;; In ANSI Common Lisp, the effects of adding a definition
;;; (or most anything else) to a symbol in the 'CL-USER' package is undefined.
;;;
;;; Therefore, it would be best to rename the function SYMBOL to something
;;; else. This has not been done (for compatibility with the book).

;; Shadows SYMBOL form 'CL-USER' package
(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun last1 (list)
  "Return the last element (not last cons cell) of LIST"
  (first (last list)))

;;; ____________________________________________________________________________
;;;                                                         Delayed Computation

(defstruct delay value (computed? nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by `force'."
  `(make-delay :value #'(lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
        (setf (delay-computed? delay) t))))

;;; ____________________________________________________________________________
;;;                                                                 Defresource

(defmacro defresource (name &key constructor (initial-copies 0)
                              (size (max initial-copies 10)))
  (let ((resource (symbol '* (symbol name '-resource*)))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(progn
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc #',deallocate (loop repeat ,initial-copies
                                    collect (,allocate))))
       ',name)))

(defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;; ____________________________________________________________________________
;;;                                                                      Queues

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert ITEM at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))

;;;; Other:

(defun sort* (seq pred &key key)
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse X-Y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of TREE satisfying PREDICATE,
with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
       predicate
       (first tree)
       (unique-find-if-anywhere predicate (rest tree)
                                found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does PREDICATE apply to any atom in the TREE?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

;;; ____________________________________________________________________________

(defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(loop for element in elements
          for i from 0
          collect `(defconstant ,element ,i))))

;;; ____________________________________________________________________________

(defun nothing (&rest args)
  "Don't do anything, and return nil."
  (declare (ignore args))
  nil)

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of X if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of X, if it is a list; else X itself."
  (if (consp x) (first x) x))
