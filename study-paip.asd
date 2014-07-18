;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; study-paip.asd

(asdf:defsystem #:study-paip
    :serial t
    :description "Source code from Peter Norvig book 'Paradigms of Artificial Intelligence Programming'"
    :author "Zlatozar Zhelyazkov <zlatozar@gmail.com>"

    :components ((:file "package")

                 ;; Helper functions and examples runner
                 (:file "auxfns" :depends-on ("package"))
                 (:file "tutor" :depends-on ("auxfns"))

                 ;; Book chapters
                 (:file "ch1/intro" :depends-on ("package"))
                 (:file "ch1/examples" :depends-on ("tutor" "ch1/intro"))))
