;;;; regulations-parser.asd

(asdf:defsystem #:regulations-parser
  :description "A parser for regulations"
  :author "Jerry Vinokurov <grapesmoker@gmail.com>"
  :license "GPL v3"
  :serial t
  :depends-on (#:mpc #:plump #:cl-ppcre #:drakma #:cl-json)
  :components ((:file "package")
               (:file "utils")
               (:file "regulations-parser")
               (:file "tree")
               (:module grammar
                        :components
                        ((:file "primitives")
                         (:file "dates")
                         (:file "markers")))))


