;;;; regulations-parser.lisp

(in-package :regulations-parser)

(defparameter +defined-terms+ (make-hash-table))
(defparameter +verbose+ t)

(push #\thin_space *whitespace*)

(defun load-xml-tree (filename)
  (strip (parse filename)))

(defun nth-child (n node)
  (aref (children node) n))

(defun parser-driver (notice-file document-number)
  (setf +defined-terms+ (make-hash-table :test #'equal))
  (let* ((notice-xml (load-xml-tree notice-file))
         (reg-meta (build-meta document-number))
         (toc (build-toc notice-xml))
         (part (build-regtext notice-xml))
         (appendices (build-appendices notice-xml))
         (interps (build-interps notice-xml))
	 (regulation-start-tag
	  (format nil "<regulation xmlns=\"eregs\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"eregs http://cfpb.github.io/regulations-schema/src/eregs.xsd\">~%"))
	 (regulation-end-tag "</regulation>"))
    (setf (part-appendices part) appendices)
    (setf (part-toc part) toc)
    (setf (part-interpretations part) interps)
    (format nil "~A~A~A~A"
	    regulation-start-tag
	    (meta->xml reg-meta 1 2)
	    (part->xml part 1 2)
	    regulation-end-tag)))

(defun parse-reg-to-xml (notice-file document-number output-file)
  (with-open-file (out output-file :direction :output
		       :if-exists :supersede :if-does-not-exist :create)
    (let ((reg-xml (parser-driver notice-file document-number)))
      (format out "~A~%" reg-xml))))
         

(defun build-regtext (notice-xml)
  (let* ((regtext-elem (first (get-elements-by-tag-name notice-xml "REGTEXT")))
         (part-number (get-attribute regtext-elem "PART"))
         (part-elem (first (get-elements-by-tag-name regtext-elem "PART")))
         (part (build-part part-elem part-number)))
    part))

(defun build-appendices (notice-xml)
  (let* ((regtext-elem (first (get-elements-by-tag-name notice-xml "REGTEXT")))
	 (part-number (get-attribute regtext-elem "PART")))
    (multiple-value-bind (appendix-contents appendix-headers appendix-letters)
	(find-appendix-contents notice-xml part-number)
      (loop
	 for app in appendix-contents
	 for letter in appendix-letters
	 for header in appendix-headers
	 collect
	   (build-appendix app part-number header letter)))))
  
(defun build-interps (notice-xml)
  ;; (declare (ignore result-root))
  (let* ((regtext-elem (first (get-elements-by-tag-name notice-xml "REGTEXT")))
         (part-number (get-attribute regtext-elem "PART"))
         (list-of-interps (extract-interp-sections notice-xml part-number))
         (interp-tree (build-interp-tree list-of-interps))
         (interpretations (make-interpretations :sections interp-tree)))
    (loop
       for section in (interpretations-sections interpretations)
       do
	 (scan-and-interpolate-termrefs-and-cites (interp-section-paragraphs section)
						  (interp-section-label section)
						  :par-type :interp))
    interpretations))

(defparameter *test-file* #p"/home/jerry/Programming/lisp/regulations-parser/712.xml")
(defparameter *reg-e-file* #p"/home/jerry/Programming/lisp/regulations-parser/2011-31725.xml")
(defparameter *reg-z-file* #p"/home/jerry/Programming/lisp/regulations-parser/2011-31715.xml")
(defparameter *reg-e* (load-xml-tree *reg-e-file*))
(defparameter *reg-z* (load-xml-tree *reg-z-file*))
(defparameter *test-tree* (load-xml-tree *test-file*))
