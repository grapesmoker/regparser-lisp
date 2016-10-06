(in-package :regulations-parser)

(defstruct appendix-section
  (section-number 0 :type number)
  (subject "" :type string)
  (label "" :type string)
  (paragraphs '() :type list))

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
    (values appendix-contents appendix-letters)))
	 
(defun build-appendix (root part letter)
  (let* ((appendix-start
	  (first (reverse (select-by (where :tag "HD") root)))))
    (loop
       with current-elem = (next-sibling appendix-start)
       with current-section = (make-appendix-section
			       :section-number 1
			       :subject (text appendix-start)
			       :label (format nil "~A-~A-~A" part letter 1))
       with appendix-sections = '()
       with sec-num = 2
       while current-elem
       do
	 (cond ((and (string= (tag-name current-elem) "HD")
		     (string= (get-attribute current-elem "SOURCE") "HD1"))
		(setf (appendix-section-paragraphs current-section)
		      (reverse (appendix-section-paragraphs current-section)))
		(push current-section appendix-sections)
		(incf sec-num)
		(setf current-section
		      (make-appendix-section
		       :section-number sec-num
		       :subject (text current-elem)
		       :label (format nil "~A-~A-~A" part letter sec-num))))
	       ((string= (tag-name current-elem) "P")
		(let ((paragraph (build-paragraph current-elem)))
		  (push paragraph (appendix-section-paragraphs current-section)))))
	 (setf current-elem (next-sibling current-elem))
       finally (return (reverse appendix-sections)))))
	 
	 
