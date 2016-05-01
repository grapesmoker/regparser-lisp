;;;; package.lisp

(defpackage #:regulations-parser
  (:nicknames regparser)
  (:use #:plump #:cl #:drakma #:cl-json
        #:mpc #:mpc.characters #:mpc.numerals
        #:cl-ppcre))

