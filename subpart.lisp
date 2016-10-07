(in-package :regulations-parser)

(defstruct subpart
  (label "" :type string)
  (letter "" :type string)
  (sections '() :type list))

(defun build-subpart (subpart-elem part-number)
  ;; (declare (optimize (debug 3)))
  (let* ((sections
	  (reverse
	   (remove-if #'null
		      (loop
			 for section in (get-elements-by-tag-name subpart-elem "SECTION")
			 collect
			   (build-section section part-number)))))
	 (letter
	  (when sections
	    (remove-if #'null
		       (let ((header (nth-child 0 subpart-elem)))
			 (run (=subpart) (text header))))))
	 (label (format nil "~A-Subpart-~A" part-number letter)))
    (when sections
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
	   (scan-and-interpolate-termrefs-and-cites (section-paragraphs section)
						    (section-label section))))
    (when (and letter label)
      (make-subpart :label label
		    :letter letter
		    :sections sections))))
	      

(defun subpart->xml (subpart &optional (depth 0) (indent 4))
  (let* ((subpart-start-tag (format nil "~A<subpart label=\"~A\">~%"
				    (indent (* depth indent))
				    (subpart-label subpart)))
	 (content-start-tag (format nil "~A<content>~%" (indent (* (+ 1 depth) indent))))
	 (content-end-tag (format nil "~A</content>~%" (indent (* (+ 1 depth) indent))))
	 (subpart-end-tag (format nil "~A</subpart>~%" (indent (* depth indent)))))
    (format nil "~A~A~{~A~}~A~A"
	    subpart-start-tag
	    content-start-tag
	    (loop
	       for section in (subpart-sections subpart)
	       collect
		 (section->xml section (+ 2 depth) indent))
	    content-end-tag
	    subpart-end-tag)))
