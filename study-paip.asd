;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; Copyright (c) 2014, 2015 Zlatozar Zhelyazkov

;;;; study-paip.asd

(in-package :cl-user)

(asdf:defsystem #:study-paip
    :name "study-paip"
    :version "1.0"
    :description "Source code from Peter Norvig's book
'Paradigms of Artificial Intelligence Programming'"
    :author "Peter Norvig <peter@norvig.com>"
    :maintainer "Zlatozar Zhelyazkov <zlatozar@gmail.com>"

    :serial t
    :components ((:file "packages")

                 ;; Helper functions
                 (:file "tools/introspection" :depends-on ("packages"))
                 (:file "tools/test" :depends-on ("packages"))
                 (:file "auxfns" :depends-on ("packages"))

                 ;; Book examples runner
                 (:file "tutor" :depends-on ("auxfns"))

                 ;; Book chapters
                 (:file "ch01/intro" :depends-on ("auxfns"))
                 (:file "ch01/examples" :depends-on ("tutor" "ch01/intro"))
                 (:file "ch01/exercises" :depends-on ("tools/test" "ch01/intro"))

                 (:file "ch02/simple")
                 (:file "ch02/examples" :depends-on ("tutor" "ch02/simple"))
                 (:file "ch02/exercises" :depends-on ("tools/test"))

                 (:file "ch03/overview")
                 (:file "ch03/examples" :depends-on ("tutor" "ch03/overview"))
                 (:file "ch03/exercises" :depends-on ("tools/test" "ch03/overview"))

                 ;; GPS: first version
                 (:file "ch04/gps1")
                 (:file "ch04/examples-gps1" :depends-on ("tutor" "ch04/gps1"))
                 ;; GPS: final version and exercises
                 (:file "ch04/gps" :depends-on ("ch04/gps1"))
                 (:file "ch04/examples-gps" :depends-on ("tutor" "ch04/gps"))
                 (:file "ch04/exercises" :depends-on ("tools/test"))

                 ;; ELIZA: first version
                 (:file "ch05/eliza1")
                 (:file "ch05/examples-eliza1" :depends-on ("tutor" "ch05/eliza1"))
                 ;; ELIZA: final version and exercises
                 (:file "ch05/eliza" :depends-on ("ch05/eliza1"))
                 (:file "ch05/examples-eliza" :depends-on ("tutor"))
                 (:file "ch05/exercises" :depends-on ("tools/test"))

                 (:file "ch06/patmatch")
                 (:file "ch06/eliza-pm" :depends-on ("ch06/patmatch" "ch05/eliza"))
                 (:file "ch06/search")
                 (:file "ch06/gps-srch" :depends-on ("ch06/search" "ch04/gps"))
                 (:file "ch06/examples" :depends-on ("tutor" "ch06/patmatch"
                                                             "ch06/eliza-pm"
                                                             "ch06/search"
                                                             "ch06/gps-srch"))
                 (:file "ch06/exercises" :depends-on ("tools/test"))

                 ))
