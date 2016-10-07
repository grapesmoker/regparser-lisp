(in-package :regulations-parser)

(defstruct interp-section
  (section-number "" :type string)
  (part-number "" :type string)
  (subject "" :type string)
  (label "" :type string)
  (title "" :type string)
  (paragraphs '() :type list))

(defun interp-section->xml (interp-section &optional (depth 0) (indent 4))
  (let* (;;(part (interp-section-part-number interp-section))
	 (label (interp-section-label interp-section))
	 (paragraphs (interp-section-paragraphs interp-section))
	 (section-start-tag
	  (format nil "~A<interpSection label=\"~A\">~%" (indent (* depth indent)) label))
	 (title
	  (format nil "~A<title>~A</title>~%"
		  (indent (* (+ depth 1) indent))
		  (interp-section-title interp-section)))
	 (section-end-tag (format nil "~A</interpSection>~%" (indent (* depth indent))))
	 (paragraph-xml-list
	  (loop
	     for par in paragraphs
	     collect
	       (interp-paragraph->xml par (+ depth 1) indent))))
    (format nil "~A~A~{~A~}~A"
	    section-start-tag
	    title
	    paragraph-xml-list
	    section-end-tag)))

(defun build-interp-section (node-list part-number)
  (let* ((section-info 
          (run (=interp-section-header part-number) 
               (text (first node-list))))
         (section-number (cdr (assoc :section section-info)))
         (interp-section
          (make-interp-section 
           :section-number section-number
           :part-number part-number
           :label (format nil "~A-~A-Interp" section-number part-number))))
    (print section-info)
    (print (rest node-list))
    (loop
       for node in (rest node-list)
       with current-par-container = nil
       do
    	 (print "foo")
         (print node)
	 (let* ((header-par (header->paragraph node part-number)))
	   (format t "header par: ~A~%" header-par)
	   (if header-par
	       (progn
		 (when current-par-container
		   (push current-par-container
			 (interp-section-paragraphs interp-section))
                 (setf current-par-container header-par)))
               (let ((paragraphs (build-paragraph node :par-mode :interp)))
                 (print paragraphs)
                 (print (text node))
                 (print current-par-container)
                 (push paragraphs (interp-paragraph-subparagraphs current-par-container)))))
       finally
	 (when current-par-container
	   (setf (interp-paragraph-subparagraphs current-par-container)
		 (reverse (flatten (interp-paragraph-subparagraphs current-par-container))))
	   (push current-par-container (interp-section-paragraphs interp-section))))
    (print "foobar")
    (setf (interp-section-paragraphs interp-section)
          (reverse (interp-section-paragraphs interp-section)))
    (print "inserting interp cites")
    (scan-and-interpolate-termrefs-and-cites (interp-section-paragraphs interp-section)
					     (interp-section-label interp-section)
					     :par-type :interp)
    interp-section))
           

(defun header->paragraph (node part-number)
  (let* ((paragraph-info (run (=interp-paragraph-header) (text node))))
    (when paragraph-info
        (let* ((section
                (cdr (assoc :section paragraph-info)))
               (markers
                (mapcar #'strip-marker (cdr (assoc :markers paragraph-info))))
               (full-text
                (cdr (assoc :full-text paragraph-info)))
               (interp-target
                (format nil "~A-~A-~{~A~^-~}" part-number section markers))
               (interp-label
                (format nil "~A-~A-~{~A~^-~}-Interp" part-number section markers)))
          (make-interp-paragraph :label interp-label
                                 :target interp-target
                                 :title full-text)))))


(defun build-interp-tree (interp-list)
  ;; (declare (optimize (debug 3)))
  (labels ((interp-par-parent-label (interp-node)
	     (let* ((label-elements (split "-" (interp-paragraph-label interp-node)))
		    (parent-elements
		     (if (string= (car (last label-elements)) "Interp")
			 (append (butlast label-elements 2) (list "Interp"))
			 (butlast label-elements))))
	       (format nil "~{~A~^-~}" parent-elements)))
	   
	   (find-parent (interp-node list-of-interps)
	     (let ((parent-label (interp-par-parent-label interp-node)))
	       (loop
		  for interp-item in list-of-interps
		  with result = nil
		  while (null result)
		  do
		    (setf result
			  (if (or (and (eq (type-of interp-item) 'interp-paragraph)
				       (string= (interp-paragraph-label interp-item) parent-label))
				  (and (eq (type-of interp-item) 'interp-section)
				       (string= (interp-section-label interp-item) parent-label)))
			      interp-item
			      (let ((children
				     (typecase interp-item
				       (interp-paragraph
					(interp-paragraph-subparagraphs interp-item))
				       (interp-section
					(interp-section-paragraphs interp-item)))))
				(find-parent interp-node children))))
		  finally (return result)))))
    (loop
       with interp-tree = '()
       with current-parent-label = nil
       with previous-parent-label = nil
       with previous-parent = nil
       for interp-item in interp-list
       do
	 ;;(format t "~A~%" interp-item)
	 (typecase interp-item
	   (interp-section
	    (push (copy-interp-section interp-item) interp-tree))
	   (interp-paragraph
	    (let ((parent (find-parent interp-item interp-tree)))
	      ;; (format t "parent: ~A~%" parent)
	      (typecase parent
		(interp-section
		 (setf current-parent-label (interp-section-label parent))
		 (setf
		  (interp-section-paragraphs parent)
		  (append (interp-section-paragraphs parent)
			  (list (copy-interp-paragraph interp-item)))))
		(interp-paragraph
		 (setf current-parent-label (interp-paragraph-label parent))
		 (setf
		  (interp-paragraph-subparagraphs parent)
		  (append (interp-paragraph-subparagraphs parent)
			  (list (copy-interp-paragraph interp-item))))))
	      (setf previous-parent parent)
	      (setf previous-parent-label current-parent-label))))
       finally (return (nreverse interp-tree)))))


(defun build-interp-hierarchy (interp-tree)
  (let ((tree-copy (copy-list interp-tree)))
    (loop
       for section in tree-copy
       do
	 (loop
	    for par in (interp-section-paragraphs section)
	    do
	      (let* ((paragraphs (interp-paragraph-subparagraphs par))
		     (markers (mapcar #'(lambda (item)
					  (strip-marker (interp-paragraph-marker item)))
				      paragraphs))
		     (hierarchy (compute-marker-hierarchy markers :generalized t))
		     (subpar-tree
		      (when paragraphs
			(build-interp-paragraph-tree paragraphs hierarchy
						     (interp-paragraph-label par)))))
		(setf (interp-paragraph-subparagraphs par) subpar-tree))))
    tree-copy))
		    
	      
	    
(defun build-interp-paragraph-tree (interp-paragraphs hierarchy label-root)
  (loop
     with last-par-at-depth = (make-hash-table)
     with top-level-pars = '()
     for par in (rest interp-paragraphs)
     for marker in (rest hierarchy)
     initially
       (setf (interp-paragraph-depth (first interp-paragraphs)) (marker-depth (first hierarchy)))
       (push (first interp-paragraphs) top-level-pars)
       (setf (gethash (marker-depth (first hierarchy)) last-par-at-depth) (first interp-paragraphs))
     do
       (let* ((depth (marker-depth marker))
	      (last-par-at-this-depth (gethash depth last-par-at-depth)))
	 (setf (interp-paragraph-depth par) depth)
	 ;; (format t "~A~%" par)
	 (cond ((and last-par-at-this-depth
		     (= depth 1))
		(progn
		  ;; (format t "found par at depth ~A~%" depth)
		  (push par top-level-pars)
		  (setf (gethash depth last-par-at-depth) par)))
	       ((and last-par-at-this-depth
		     (> depth 1))
		(let ((last-par-at-prev-depth (gethash (- depth 1) last-par-at-depth)))
		  ;; (format t "found par at depth ~A~%" depth)
		  (push par (interp-paragraph-subparagraphs last-par-at-prev-depth))
		  (setf (gethash depth last-par-at-depth) par)))
	       ((null last-par-at-this-depth)
		(let ((last-par-at-prev-depth (gethash (- depth 1) last-par-at-depth)))
		  ;; (format t "no par at depth ~A~%" depth)
		  (push par (interp-paragraph-subparagraphs last-par-at-prev-depth))
		  (setf (gethash depth last-par-at-depth) par)))))
     finally
       (labels ((recursive-reverse-and-label (list-of-pars par-label-root)
		  (loop
		     for par in list-of-pars
		     for index downfrom (length list-of-pars)
		     do
		       (let ((par-label
			      (if (not (string= (interp-paragraph-marker par) ""))
				  (format nil "~A-~A" par-label-root
					  (strip-marker (interp-paragraph-marker par)))
				  (format nil "~A-p~D" par-label-root index))))
			 (setf (interp-paragraph-label par) par-label)
			 (setf (interp-paragraph-subparagraphs par)
			       (reverse (interp-paragraph-subparagraphs par)))
			 (recursive-reverse-and-label
			  (interp-paragraph-subparagraphs par) par-label)))))
	 (recursive-reverse-and-label top-level-pars label-root)
	 (return (reverse top-level-pars)))))
