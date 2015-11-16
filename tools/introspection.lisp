;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INSPECT; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; File introspection.lisp: Miscellaneous functions that could be used to understand
;;;; PAIP code.

(in-package #:inspect)


;;; ____________________________________________________________________________
;;;                                                            Helper functions

(defun require-once (string)
  (let ((tried (load-time-value
                (make-hash-table :test #'equal :size 8))))
       (or (find-package string)
           (unless (gethash string tried)
             (setf (gethash string tried) t)
             (ignore-errors (require string))
             (find-package string)))))

(defun arglist (fname)
  "Return the arglist for the given function name. SBCL only."
  (if (require-once "SB-INTROSPECT")
       (sb-introspect:function-lambda-list fname)
       :failed-to-load-sb-introspect))

(defun ensure-unquoted (form)
  "If FORM is quoted, remove one level of quoting. Otherwise return FORM.
This is a useful for defining convenience for macros which may be passed a
quoted or unquoted symbol."
  (if (and (listp form) (eq (car form) 'cl:quote))
      (second form)
      form))

(defun string-sort (list)
  (sort (copy-list list) #'string<))

(defun print-asdf-description (package)
  (let ((description (ignore-errors
                       (funcall [asdf system-description]
                        (funcall [asdf find-system]
                         (string-downcase (package-name
                                           package)))))))
    (when description
      (format t "~&~A > ASDF System~% ~<~A~%~%~>"
              (package-name package) description))))

;;; ____________________________________________________________________________
;;;                                                             Examine symbols

(defparameter *documentation-types*
  '(function setf type variable compiler-macro method-combination)
  "Types that might work with (documentation obj type)")

(defmacro ?? (symbol &rest ignored-arguments)
  "Print any documentation for the symbol.
Includes variable, function, type, compiler macro, method
 combination, and setf documentation."
  (declare (ignore ignored-arguments))
  `(doc% ',(ensure-unquoted symbol)))

(defun doc% (symbol)
  (do ()
      ((not (consp symbol)))
    (setq symbol (car symbol)))
  (let ((*print-case* :downcase))
    (dolist (type *documentation-types*)
      (when (documentation symbol type)
	(if (member type '(compiler-macro function setf
			   #-clisp method-combination))
	    (format t "~&(~:@(~A~)~@[~{ ~A~}~]) > ~A~% ~<~A~%~%~>"
		    symbol
		    (when #1=(arglist symbol)
			  (if (consp #1#) #1# (list #1#)))
		    (if (macro-function symbol)
			'macro
			type)
		    (documentation symbol type))
	    (format t "~&~A > ~A~% ~<~A~%~%~>"
		    symbol
		    type
		    (documentation symbol type)))))))

;; Or just type 'C-d C-d' in Slime
(defmacro !! (&rest rest)
  "Describe REST using `describe'"
  `(apply #'describe (quote ,rest))) ; see `ch9:kwote'

;;; ____________________________________________________________________________
;;;                                                            Examine packages

(defmacro ?p (&optional (package *package*))
  "Print the documentation on the exported symbols of a package."
  `(readme% ',(ensure-unquoted package)))

(defun readme% (&optional (package *package*))
  (let (undocumented-symbols
        documented-symbols)
    (terpri)
    (when (documentation (find-package package) t)
      (format t "~&~A > Package~% ~<~A~%~%~>"
              package
              (documentation (find-package package) t)))
    (print-asdf-description package)
    (do-external-symbols (sym package)
      (if (some (lambda (doctype) (documentation sym doctype))
                *documentation-types*)
          (push sym documented-symbols)
          (push sym undocumented-symbols)))
    (when undocumented-symbols
      (format t "~&Undocumented exported symbols:~%~% ~{~A ~}~%~%~
                   Documented exported symbols:~%~%"
              (string-sort undocumented-symbols)))
    (dolist (sym (string-sort documented-symbols))
      (doc% sym))))

(defmacro ?p% (string-designator)
  "Print all package names and nicknames which contain the given string."
  `(package-apropos% ',(ensure-unquoted string-designator)))

(defun package-apropos% (string-designator)
  (let ((string (string string-designator))
        nicknamep)
    (dolist (p (list-all-packages) (values))
      (setq nicknamep nil)
      (dolist (name (cons (package-name p)
                          (package-nicknames p)))
        (when (search string name :test #'char-equal)
          (format t "~&~A~30,5t~@[(Nickname of ~A)~]~&" name nicknamep))
        (unless nicknamep (setq nicknamep name))))))

(defun ?p* ()
  "Return a list of all package names."
  (let ((reslist (mapcar #'package-name
                         (list-all-packages))))
    (format t "~{~S,~% ~}" (sort reslist #'string-lessp))))

;;; ____________________________________________________________________________
;;;                                                              Examine macros

(defmacro ?mac (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((*gensym-counter* 0)
          (exp1 (macroexpand-1 ',(ensure-unquoted form)))
	  (exp (macroexpand exp1))
          (*print-case* :downcase)
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion~%:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:~%")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))
