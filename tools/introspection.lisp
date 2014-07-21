;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; File introspection.lisp: Miscellaneous functions that could be used to understand PAIP code.

(in-package #:tools)

;;; ____________________________________________________________________________
;;;                                                             Examine symbols

(defun ?a (&optional (substring ""))
  "Return a list of all symbols containing SUBSTRING as a substring."
  (sort (apropos-list substring) #'string-lessp))

;;; The ?X functions retrieve some informations about specific symbols

(defun ?? (some-symbol)
  "Verbosely describe a symbol using #'describe"
  (describe some-symbol))

(defun ?f (some-function)
  "Documentation string of a function"
  (documentation some-function 'function))

(defun ?v (some-variable)
  "Documentation string of a variable"
  (documentation some-variable 'variable))

(defun ?t (some-type)
  (documentation some-type 'type))

(defun ?s (some-structure)
  (documentation some-structure 'structure))

;;; ____________________________________________________________________________
;;;                                                            Examine packages

;;; This is a helper macro for the followig PACKAGE-*-SYMBOLS functions.

(defmacro loop-as-package (package symbol-type)
  `(loop
      for symname being each ,symbol-type of ,package
      collect symname into reslist
      finally
        (return (setf reslist
                      (sort reslist #'string-lessp)))))

(defun package-all-symbols (&optional (package "COMMON-LISP-USER"))
  "Return a list of symbols in PACKAGE name."
  (loop-as-package package symbol))

(defun package-internal-symbols (&optional (package "COMMON-LISP-USER"))
  "Return a list of internal symbols in PACKAGE name."
  (loop-as-package package present-symbol))

(defun package-external-symbols (&optional (package "COMMON-LISP-USER"))
  "Return a list of external symbols in PACKAGE name."
  (loop-as-package package external-symbol))

;;; NAME-ALL-PACKAGES returns a list of ALL available package (names).

(defun name-all-packages ()
  "Return a list of all package names."
  (let ((reslist (mapcar #'package-name
                         (list-all-packages))))
    (sort reslist #'string-lessp)))

(defun package-describe-string (&optional (package "COMMON-LISP-USER"))
  "Return a non-portable description for PACKAGE name, via DESCRIBE."
  (with-output-to-string (sstream)
    (describe (find-package package) sstream)))
