;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH12-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch12-exercises)

;;; ____________________________________________________________________________


(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))

(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      (0 fail)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (gensym "F")))
           `(flet ((,fn () ,(compile-body body cont bindings)))
              .,(maybe-add-undo-bindings
                 (loop for g in disjuncts collect
                      (compile-body (list g) `#',fn
                                    bindings)))))))))

;; when a goal succeeds, we call the continuation
(defun true/0 (cont)
  (funcall cont))

;; when a goal fails, we ignore the continuation
(defun fail/0 (cont)
  (declare (ignore cont))
  nil)

;; Prolog trace

(defvar *prolog-trace-indent* 0)

(defun prolog-trace (kind predicate &rest args)
  (if
   (member kind '(call redo))
   (incf *prolog-trace-indent* 3))
  (format t "~&~VT~a ~a:~{ ~a~}"
          *prolog-trace-indent* kind predicate args)
  (if
   (member kind '(fail exit))
   (decf *prolog-trace-indent* 3)))

(defun >/2 (x y cont)
       (if
        (and (numberp (deref x)) (numberp (deref y)) (> χ y))
        (funcall cont)))

(defun numberp/1 (x cont)
  (if
   (numberp (deref x ) )
   (funcall cont)))
