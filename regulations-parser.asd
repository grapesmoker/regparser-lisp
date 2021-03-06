;;;; regulations-parser.asd

(asdf:defsystem #:regulations-parser
  :description "A parser for regulations"
  :author "Jerry Vinokurov <grapesmoker@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:mpc #:plump #:cl-ppcre #:drakma #:cl-json #:log4cl)
  :components ((:file "package")
               (:file "utils")
               (:file "meta")
               (:file "toc")
               (:file "regulations-parser")
               (:file "tree")
               (:module grammar
                        :components
                        ((:file "primitives")
                         (:file "dates")
                         (:file "markers")
                         (:file "terms")
                         (:file "citations")
                         (:file "interps")))
               (:file "hierarchy")
               (:file "paragraphs")
               (:file "sections")
               (:file "subpart")
               (:file "part")
	       (:file "appendices")
               (:file "interpretations")
               (:file "interp-paragraph")
               (:file "interp-section")))
               
