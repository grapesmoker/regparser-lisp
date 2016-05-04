;;;; regulations-parser.lisp

(in-package :regulations-parser)

(defun load-xml-tree (filename)
  (strip (parse filename)))

(defun nth-child (n node)
  (aref (children node) n))

(defun parser-driver (notice-file document-number)
  (let* ((notice-xml (load-xml-tree notice-file))
         (root-result (make-root))
         (reg-meta (build-meta root-result document-number))
         (toc-xml (build-toc notice-xml root-result))
         (regtext (build-regtext notice-xml root-result reg-meta))
         (appendices (build-appendices notice-xml root-result))
         (interps (build-interps notice-xml root-result)))
    regtext))
         

(defun build-regtext (notice-xml result-root reg-meta)
  (let* ((regtext-elem (first (get-elements-by-tag-name notice-xml "REGTEXT")))
         (part-number (get-attribute regtext-elem "PART"))
         (part-elem (first (get-elements-by-tag-name regtext-elem "PART")))
         (part (build-part part-elem part-number)))
    part))

(defun build-appendices (notice-xml result-root)
  ())

(defun build-interps (notice-xml result-root)
  ())

(defparameter *test-file* #p"/home/jerry/Programming/lisp/regulations-parser/712.xml")
