(in-package :regulations-parser)


(defstruct toc
  (sections '() :type list)
  (appendices '() :type list))

(defstruct toc-sec-entry
  (target "" :type string)
  (section-num "" :type string)
  (section-subject "" :type string))

(defstruct toc-app-entry
  (target "" :type string)
  (appendix-letter "" :type string)
  (appendix-subject "" :type string))

(defun toc-sec-entry->xml (entry &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<tocSecEntry target=\"~A\">~%"
			   (indent (* depth indent))
			   (toc-sec-entry-target entry)))
	(end-tag (format nil "~A</tocSecEntry>~%" (indent (* depth indent))))
	(section-num
	 (format nil "~A<sectionNum>~A</sectionNum>~%"
		 (indent (* (+ 1 depth) indent))
		 (toc-sec-entry-section-num entry)))
	(section-subject
	 (format nil "~A<sectionSubject>~A</sectionSubject>~%"
		 (indent (* (+ 1 depth) indent))
		 (toc-sec-entry-section-subject entry))))
    (format nil "~A~A~A~A" start-tag section-num section-subject end-tag)))

(defun toc-app-entry->xml (entry &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<tocAppEntry target=\"~A\">~%"
			   (indent (* depth indent))
			   (toc-app-entry-target entry)))
	(end-tag (format nil "~A</tocAppEntry>~%" (indent (* depth indent))))
	(appendix-letter
	 (format nil "~A<appendixLetter>~A</appendixLetter>~%"
		 (indent (* (+ 1 depth) indent))
		 (toc-app-entry-appendix-letter entry)))
	(appendix-subject
	 (format nil "~A<appendixSubject>~A</appendixSubject>~%"
		 (indent (* (+ 1 depth) indent))
		 (toc-app-entry-appendix-subject entry))))
    (format nil "~A~A~A~A" start-tag appendix-letter appendix-subject end-tag)))
  
(defun toc->xml (toc &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<tableOfContents>~%" (indent (* depth indent))))
	(end-tag (format nil "~A</tableOfContents>~%" (indent (* depth indent))))
	(section-xml
	 (loop
	    for section in (toc-sections toc)
	    collect
	      (toc-sec-entry->xml section (+ 1 depth) indent)))
	(appendix-xml
	 (loop
	    for appendix in (toc-appendices toc)
	    collect
	      (toc-app-entry->xml appendix (+ 1 depth) indent))))
    (format nil "~A~{~A~}~{~A~}~A"
	    start-tag section-xml appendix-xml end-tag)))
    

(defun build-toc (notice-root)
  (let* ((regtext (select-by (where :tag "REGTEXT") notice-root))
         (part-number (get-attribute regtext "PART"))
         (part (select-by (where :tag "PART") regtext))
         (contents (select-by (where :tag "CONTENTS") part))
         (section-numbers (nreverse (select-by (where :tag "SECTNO") contents)))
         (section-subjects (nreverse (select-by (where :tag "SUBJECT") contents)))
         (appendices (nreverse (select-by
				(where :tag "FP" :attribute '("SOURCE" "FP-2")) notice-root)))
         (table-of-contents
	  (make-toc :sections
		    (remove-if
		     #'null
		     (loop
			for sec-num in section-numbers
			for sec-sub in section-subjects
			collect
			  (let* ((target (regex-replace "\\." (text sec-num) "-"))
				 (section-num (second (split "-" target)))
				 (section-subject (text sec-sub)))
			    (make-toc-sec-entry :target target
						:section-num section-num
						:section-subject section-subject))))
		    :appendices
		    (remove-if
		     #'null
		     (loop
			for appendix-elem in appendices
			collect
			  (let* ((appendix-subject (text appendix-elem))
				 (appendix-letter (cdr (run (=appendix) appendix-subject))))
			    (when appendix-letter
			      (make-toc-app-entry :target (format nil "~D-~A"
								  part-number appendix-letter)
						  :appendix-letter appendix-letter
						  :appendix-subject appendix-subject))))))))
    table-of-contents))
