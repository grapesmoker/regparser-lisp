(in-package :regulations-parser)

(defstruct appendix-section
  (section-number 0 :type number)
  (subject "" :type string)
  (label "" :type string)
  (paragraphs '() :type list))

(defstruct appendix
  (appendix-letter "" :type string)
  (label "" :type string)
  (title "" :type string)
  (sections '() :type list))

(defun =appendix-to-part (part)
  (=let* ((_ (=or (=string "Appendix")
		  (=string "appendix")))
	  (appendix-letter
	   (=skip-whitespace
	    (=satisfies #'(lambda (ch)
			    (find ch *uppercase-letters* :test #'string=)))))
	  (_ (=skip-whitespace
	      (=string "to Part")))
	  (_ (=skip-whitespace
	      (=string part))))
    (=result (stringify appendix-letter))))

(defun find-appendix-headers (root part)
  (let* ((headers (select-by (where :tag "HD" :attribute (cons "SOURCE" '("HD1"))) root)))
    (reverse
     (loop
	for hd in headers
	if (run (=appendix-to-part part) (text hd))
	collect hd))))

(defun find-appendix-contents (root part)
  (let* ((appendix-headers (find-appendix-headers root part))
	 (appendix-letters
	  (loop
	     for hd in appendix-headers
	     collect
	       (run (=appendix-to-part part) (text hd))))
	 (appendix-contents
	  (loop
	     for hd in appendix-headers
	     collect
	       (loop
		  with current-elem = (next-sibling hd)
		  until (string= (tag-name current-elem) "EXTRACT")
		  do
		    (setf current-elem (next-sibling current-elem))
		  finally (return current-elem)))))
    (values appendix-contents (mapcar #'text appendix-headers) appendix-letters)))

(defun appendix-has-sections (appendix-root)
  (if (select-by (where :tag "HD") appendix-root)
      t nil))

(defun appendix-has-header (appendix-root)
  (if (and (appendix-has-sections appendix-root)
	   (first (reverse (select-by (where :tag "HD") appendix-root))))
      t nil))

(defun get-appendix-start-element (appendix-root)
  (loop
     for child across (children appendix-root)
     if (or (string= (tag-name child) "HD")
	    (string= (tag-name child) "P"))
     do (return child)))

(defun compute-appendix-section-hierarchy (appendix-section)
  (let* ((paragraphs (reverse (appendix-section-paragraphs appendix-section)))
	 (markers (mapcar #'(lambda (par) (strip-marker (paragraph-marker par))) paragraphs))
	 (hierarchy (compute-marker-hierarchy markers))
	 (label (appendix-section-label appendix-section)))
    (when hierarchy
      (build-paragraph-tree paragraphs hierarchy label))))
       


(defun build-appendix (root part header letter)
  ;; (declare (optimize (debug 3)))
  (let* ((appendix-start (get-appendix-start-element root))
	 (start-tag (tag-name appendix-start)))
    (loop
       with current-elem = appendix-start
       with current-section = (make-appendix-section
			       :section-number 1
			       :subject (if (string= start-tag "HD")
					    (text appendix-start)
					    "")
			       :label (format nil "~A-~A-~A" part letter 1))
       with appendix-sections = '()
       with sec-num = (if (string= start-tag "HD") 2 1)
       while current-elem
       do
	 (cond ((and (string= (tag-name current-elem) "HD")
		     (string= (get-attribute current-elem "SOURCE") "HD1"))
		(let ((paragraph-tree (compute-appendix-section-hierarchy current-section)))
		  (setf (appendix-section-paragraphs current-section) paragraph-tree)
		  (push current-section appendix-sections)
		  (incf sec-num)
		  (scan-and-interpolate-termrefs-and-cites
		   (appendix-section-paragraphs current-section)
		   (appendix-section-label current-section))
		  (setf current-section
			(make-appendix-section
			 :section-number sec-num
			 :subject (text current-elem)
			 :label (format nil "~A-~A-~A" part letter sec-num)))))
	       ((string= (tag-name current-elem) "P")
		(let ((paragraph (build-paragraph current-elem)))
		  (if (typep paragraph 'list)
		      (loop for par in paragraph do
			   (push par (appendix-section-paragraphs current-section)))
		      (push paragraph (appendix-section-paragraphs current-section))))))
	 (setf current-elem (next-sibling current-elem))
       finally
	 (let ((paragraph-tree (compute-appendix-section-hierarchy current-section)))
	   (setf (appendix-section-paragraphs current-section) paragraph-tree)
	   (push current-section appendix-sections)
	   (return (make-appendix
		    :appendix-letter letter
		    :title header
		    :label (format nil "~A-~A" part letter)
		    :sections (reverse appendix-sections)))))))
 

(defun appendix->xml (app &optional (depth 0) (indent 4))
  (let* ((label (appendix-label app))
	 (letter (appendix-appendix-letter app))
	 (appendix-start-tag
	  (format nil "~A<appendix appendixLetter=\"~A\" label=\"~A\">~%"
		  (indent (* depth indent))
		  letter
		  label))
	 (appendix-end-tag (format nil "~A</appendix>~%" (indent (* depth indent)))))
    (format nil "~A~{~A~}~A"
	    appendix-start-tag
	    (loop
	       for app-section in (appendix-sections app)
	       collect
		 (appendix-section->xml app-section (+ 1 depth) indent))
	    appendix-end-tag)))
	  
	 
(defun appendix-section->xml (app-section &optional (depth 0) (indent 4))
  (let* ((label (appendix-section-label app-section))
	 (paragraphs (appendix-section-paragraphs app-section))
	 (section-start-tag
	  (format nil "~A<appendixSection label=\"~A\">~%" (indent (* depth indent)) label))
	 (title
	  (format nil "~A<title>~A</title>~%"
		  (indent (* (+ depth 1) indent))
		  (appendix-section-subject app-section)))
	 (section-end-tag (format nil "~A</appendixSection>~%" (indent (* depth indent))))
	 (paragraph-xml-list
	  (loop
	     for par in paragraphs
	     collect
	       (paragraph->xml par (+ depth 1) indent))))
    (format nil "~A~A~{~A~}~A"
	    section-start-tag
	    title
	    paragraph-xml-list
	    section-end-tag)))
