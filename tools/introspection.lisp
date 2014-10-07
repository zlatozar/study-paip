;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INSPECT; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; File introspection.lisp: Miscellaneous functions that could be used to understand
;;;; PAIP code.

(in-package #:inspect)

;;; ____________________________________________________________________________
;;;                                                             Examine symbols

(defvar *system-packages*
  '("SB-" "SWANK" "UIOP""QL-" "ASDF" "QUICKLISP" "BORDEAUX-" "HTML-TEMPLATE" "::")
  "Packages that are not helpful during PAIP study.")

(defun system-symbol-p (aprop-symbol)
  "Checks if APROP-SYMBOL is exposed and if it is part of system
packages."
  (let ((system-list *system-packages*))
    (some 'numberp (mapcar #'(lambda (x) (search x (format nil "~S" aprop-symbol)))
                           system-list))))

(defun start-with-p (aprop-symbol)
  "Checks if APROP-SYMBOL is a keyword."
  (eql (search ":" (format nil "~S" aprop-symbol)) 0))

(defun spec-constraint-p (aprop-symbol)
  "Predicate used to filter apropos list."
  (or (system-symbol-p aprop-symbol)
      (start-with-p aprop-symbol)))

(defun ?a (&optional (sub-string ""))
  "Return a list of all symbols containing SUB-STRING as a substring."
  (format t "~{~S,~% ~}" (sort
                          (remove-if #'spec-constraint-p (apropos-list sub-string))
                          #'string-lessp)))

(defun ?? (some-symbol)
  "Verbosely describe a symbol."
  (describe some-symbol))

;;; ____________________________________________________________________________
;;;                                                            Examine packages

(defun ?p~ (&optional (package (package-name *package*)))
  "Return a list of internal symbols in PACKAGE name.
If PACKAGE is not specified, internal symbols of current
package will be displayed."
  (let ((rslt nil))
    (do-symbols (s package)
      (when (eq (second
                 (multiple-value-list
                  (find-symbol (symbol-name s) package)))
                :internal)
        (push s rslt)))
    (format t "~{~S,~% ~}" (sort rslt #'string-lessp))))

(defun ?p+ (&optional (package (package-name *package*)))
  "Return a list of external symbols in PACKAGE name.
If PACKAGE is not specified, external symbols of current
package will be displayed."
  (let ((rslt nil))
    (do-external-symbols (s package)
      (push s rslt))
    (format t "~{~S,~% ~}" (sort rslt #'string-lessp))))

(defun ?p* ()
  "Return a list of all package names."
  (let ((reslist (mapcar #'package-name
                         (list-all-packages))))
    (format t "~{~S,~% ~}" (sort reslist #'string-lessp))))

(defun ?p% (&optional (package (package-name *package*)))
  "Return a non-portable description for PACKAGE name, via `describe'."
  (with-output-to-string (sstream)
    (describe (find-package package) sstream)))

;;; ____________________________________________________________________________
;;;                                                              Examine macros

(defun ensure-unquoted (form)
  "If FORM is quoted, remove one level of quoting. Otherwise return FORM.
This is a useful for defining convenience for macros which may be passed a
quoted or unquoted symbol."
  (if (and (listp form) (eq (car form) 'cl:quote))
      (second form)
      form))

(defmacro ?mac (expr)
  "Bind *gensym-counter* to 0, macroexpand-1 the EXPR and pprint result.
If expression starts with a quotation, unquotes it first."
  `(let ((*gensym-counter* 0)
	 (*print-case* :downcase))
     (pprint (macroexpand-1 ',(ensure-unquoted expr)))))
