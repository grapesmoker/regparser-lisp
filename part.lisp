(in-package :regulations-parser)

(defstruct part
  (part-number "" :type string)
  (toc (make-toc) :type toc)
  (subparts '() :type list)
  (appendices '() :type list)
  (interpretations nil))

(defun part->xml (part &optional (depth 0) (indent 4))
  (let* ((label (part-part-number part))
	 (part-start-tag (format nil "~%~A<part label=\"~A\">~%" (indent (* depth indent)) label))
	 (content-start-tag (format nil "~%~A<content>~%" (indent (* (+ 1 depth) indent))))
	 (content-end-tag (format nil "~A</content>~%" (indent (* (+ 1 depth) indent))))
	 (part-end-tag (format nil "~A</part>~%" (indent (* depth indent))))
	 (toc (toc->xml (part-toc part) (+ 1 depth) indent))
	 (subpart-list
	  (loop
	     for sp in (part-subparts part)
	     collect
	       (subpart->xml sp (+ depth 2) indent)))
	 (appendix-list
	  (loop
	     for ap in (part-appendices part)
	     collect
	       (appendix->xml ap (+ depth 2) indent)))
	 (interps
	  (interpretations->xml (part-interpretations part) (+ depth 2) indent)))
    (format nil "~A~A~A~{~A~}~{~A~}~A~A~A"
	    part-start-tag
	    toc
	    content-start-tag
	    subpart-list
	    appendix-list
	    interps
	    content-end-tag
	    part-end-tag)))
	   

(defun build-part (part-elem part-number)
  (let* ((subpart-elements (get-elements-by-tag-name part-elem "SUBPART"))
         (section-elements
          (unless subpart-elements
            (get-elements-by-tag-name part-elem "SECTION")))
         (subparts
	  (reverse
	   (remove-if #'null
		      (loop
			 for subpart-elem in subpart-elements
			 collect
			   (build-subpart subpart-elem part-number)))))
         (sections
          (reverse
           (loop
	      for section-elem in section-elements
	      collect
		(build-section section-elem part-number))))
         (dummy-subpart
          (unless subparts
            (list
             (make-subpart :label (format nil "~D-Subpart" part-number)
                           :sections sections)))))
    (cond (dummy-subpart
	   ;; first scan the entire tree for definitions
	   (loop
	      for section in sections
	      do
		(find-and-interpolate-definitions (section-paragraphs section)))
	   ;; now that the defined terms are in the hash, scan and
	   ;; interpolate references to those terms
	   (loop
	      for section in sections
	      do
		(scan-and-interpolate-termrefs-and-cites
		 (section-paragraphs section)
		 (section-label section)))
	   (make-part :part-number part-number
		      :subparts dummy-subpart))
	  (subparts
	   (make-part :part-number part-number
		      :subparts subparts))
	  (t (error "Subpart construction broke!")))))
    

