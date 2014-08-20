;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; study-paip.asd

(asdf:defsystem #:study-paip
    :name "study-paip"
    :version "1.0"
    :description "Source code from Peter Norvig book 'Paradigms of Artificial Intelligence Programming'"
    :author "Peter Norvig <peter@norvig.com>"
    :maintainer "Zlatozar Zhelyazkov <zlatozar@gmail.com>"

    :serial t
    :components ((:file "package")

                 ;; Helper functions
                 (:file "tools/introspection" :depends-on ("package"))
                 (:file "tools/test" :depends-on ("package"))
                 (:file "auxfns" :depends-on ("package"))

                 ;; Book examples runner
                 (:file "tutor" :depends-on ("auxfns"))

                 ;; Book chapters
                 (:file "ch01/intro" :depends-on ("auxfns"))
                 (:file "ch01/examples" :depends-on ("tutor" "ch01/intro"))
                 (:file "ch01/exercises" :depends-on ("tools/test" "ch01/intro"))

                 (:file "ch02/simple")
                 (:file "ch02/examples" :depends-on ("tutor" "ch02/simple"))
                 (:file "ch02/exercises" :depends-on ("tools/test" "ch02/simple"))

                 (:file "ch03/overview")
                 (:file "ch03/examples" :depends-on ("tutor" "ch03/overview"))
                 (:file "ch03/exercises" :depends-on ("tools/test" "ch03/overview"))

                 ;; first version
                 (:file "ch04/gps1")
                 (:file "ch04/examples-gps1" :depends-on ("tutor" "ch04/gps1"))
                 ;; final version
                 (:file "ch04/gps" :depends-on ("ch04/gps1"))
                 (:file "ch04/examples-gps" :depends-on ("tutor" "ch04/gps"))

                 ))
