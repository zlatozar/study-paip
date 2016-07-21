;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CH12-EXERCISES; Base: 10 -*-

;;;; File exercises.lisp

(in-package #:ch12-exercises)

;;; ____________________________________________________________________________


;; (def-prolog-compiler-macro lisp (goal body cont bindings)
;;   "lisp/1 and lisp/2"
;;   (let ((args (args goal)))
;;     (case (length args)
;;       (1                                ; lisp/1
;;          (let* ((lisp-exp (first args))
;;                 (lisp-args (variables-in lisp-exp)))
;;            `(progn
;;               (apply #'(lambda ,lisp-args ,(insert-deref lisp-exp))
;;                      ,(compile-arg lisp-args bindings))
;;               ,(compile-body body cont bindings))))
;;       (2                                ; lisp/2
;;          (let* ((var (first args))
;;                 (lisp-exp (second args))
;;                 (lisp-args (variables-in lisp-exp)))
;;            (compile-if
;;             `(unify! ,(compile-arg var bindings)
;;                      (apply #'(lambda ,lisp-args ,(insert-deref lisp-exp))
;;                             ,(compile-arg lisp-args bindings)))
;;             (compile-body body cont (bind-new-variables bindings goal)))))
;;       (t :pass))))

;; (defun fail/0 (cont)
;;   (declare (cl:ignore cont))
;;   nil)

;; (defun true/0 (cont)
;;   (funcall cont))

;; (defun !/0 (cont)
;;   (funcall cont))
