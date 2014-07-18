;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; package.lisp: Define packages

;;______________________________________
;;                     Examine packages

(defun package-internal-symbols (package)
  (let ((rslt nil))
    (do-symbols (s package)
                (when (eq (second
                           (multiple-value-list
                            (find-symbol (symbol-name s) package)))
                          :internal)
                  (push s rslt)))
    rslt))

(defun package-external-symbols (package)
  (let ((rslt nil))
    (do-external-symbols (s package)
                         (push s rslt))
    rslt))

;;______________________________________
;;                Book helper functions

;; Helper functions
(defpackage #:paip
  (:use #:cl)
  (:shadow #:symbol
           #:debug)

  (:export #:starts-with
           #:mappend))

;;______________________________________
;;                        Book chapters

(defpackage #:ch1
  (:use #:cl)
  (:export #:first-name
           #:last-name))



;;______________________________________
;;                         Run examples

(defpackage #:tutor
  (:use #:cl
        #:paip
        #:ch1))
