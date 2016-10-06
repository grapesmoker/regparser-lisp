(in-package :regulations-parser)

(defstruct interpretations
  (sections '() :type list))

(defun find-interp-header (root part-number)
  (select-by #'(lambda (node)
                 (and (scan "HD" (tag-name node))
                      (run (=interp-header part-number) (text node))))
             root))

;; (defun extract-interp-sections (root part-number)
;;   (let* ((interp-header (find-interp-header root part-number))
;;          (interp-extract (next-sibling interp-header)))
;;     (loop
;;        for interp-header in (reverse 
;;                              (select-by
;; 			      #'(lambda (node)
;; 				  (and (not (string= (tag-name node) "EXTRACT"))
;; 				       (run (=interp-section-header part-number)
;; 					    (text node))))
;; 			      interp-extract))
;;        ;;do
;; 	 ;;(format t "interp header: ~A~%" (text interp-header))
;;        collect
;;          (loop
;;             with current-node = (next-sibling interp-header)
;;             while (and (not (null current-node))
;;                        (not (string= "HD" (tag-name current-node))))
;;             collect current-node into interp-section-nodes
;;             do
;;               (setf current-node (next-sibling current-node))
;;             finally
;;               (return (append (list interp-header) interp-section-nodes))))))

(defun extract-interp-sections (root part-number)
  (let* ((interp-header (find-interp-header root part-number))
	 (interp-extract (next-sibling interp-header)))
    (loop
       with current-node = (nth-child 0 interp-extract)
       with current-label = part-number
       while (not (null current-node))
       collect
	 (let* ((tag (tag-name current-node))
		(node-text (text current-node))
		(intro (run (=intro) node-text))
		(subpart-letter (run (=interp-subpart) node-text))
		(section-header (run (=interp-section-header part-number) node-text))
		(paragraph-header (run (=interp-paragraph-header) node-text)))
	   (cond (intro
		  (setf current-label (format nil "~A-Interp" part-number))
		  (make-interp-section :label current-label))
		 (subpart-letter
		  (setf current-label (format nil "~A-Subpart-~A-Interp"
					      part-number subpart-letter))
		  (make-interp-section :label current-label))
		 (section-header
		  (setf current-label (format nil "~A-~A-Interp"
					      (cdr (assoc :part section-header))
					      (cdr (assoc :section section-header))))
		  (make-interp-section :section-number (cdr (assoc :section section-header))
				       :label current-label
				       :title node-text))
		 (paragraph-header
		  (setf current-label
			(format nil "~A-~A-~{~A~^-~}-Interp"
				part-number
				(cdr (assoc :section paragraph-header))
				(mapcar #'strip-marker (cdr (assoc :markers paragraph-header)))))
		  (make-interp-paragraph :marker (car (last (cdr (assoc :markers paragraph-header))))
					 :label current-label
					 :target (subseq current-label
							 0 (- (length current-label) 7))))
		 ((string= tag "P")
		  (let* ((par-marker (run (=interp-marker) node-text))
			 (par-label (format nil "~A-~A" current-label (strip-marker par-marker)))
			 (par (first (build-paragraph current-node :par-mode :interp))))
		    (setf (interp-paragraph-label par) par-label)
		    par))))
			 
       into interp-paragraphs
       do (setf current-node (next-sibling current-node))
       finally (return interp-paragraphs))))

       ;; (cond ((and (string= (tag-name current-node) "HD")
	 ;; 	     (string= (get-attribute current-node "SOURCE") "HD1"))
	 ;; 	(let ((section-header
	 ;; 	       (run (=interp-section-header part-number) (text current-node)))
	 ;; 	      (intro (run (=string "Introduction") (text current-node))))
	 ;; 	  (cond ((and section-header (not intro))
	 ;; 		 (setf current-label
	 ;; 		       (format nil "~A-~A-Interp"
	 ;; 			       current-label
	 ;; 			       (cdr (assoc :section) section-header)))
	 ;; 		 (make-interp-paragraph :label current-label))
	 ;; 		((and intro (not section-header))
	 ;; 		 (setf current-label
	 ;; 		       (format nil "~A-Interp" current-label)))
	 ;; 		(t (error "both section header and intro present!")))))
	 ;;       ((and (string= (tag-name current-node) "HD")
	 ;; 	     (string= (get-attribute current-node "SOURCE") "HD2"
		


	 ;; 	(string= (tag-name current-node) "P")
	 ;; 	(let* ((marker (run (=interp-marker) (text current-node))))
	 ;; 	  (make-interp-paragraph :marker (strip-marker marker)

;; (defun extract-interp-pars-at-level (start-node level)
;;   (loop
;;      with current-node = (next-sibling start-node)
;;      with nodes-at-level = '()
;;      while (not (null current-node))
;; ;;		(not (scan (format nil "HD~D" level)
;; ;;			   (get-attribute current-node "SOURCE"))))
;;      collect
;;        (let* ((source (format nil "HD~D" level))
;; 	      (next-source (format nil "HD~D" (+ level 1)))
;; 	      (prev-source (format nil "HD~D" (- level 1))))
;; 	 (cond ((string= (tag-name current-node) "P")
;; 		(push current-node nodes-at-level))
;; 	       ((string= (get-attribute current-node "SOURCE") next-source)
;; 		(extract-interp-pars-at-level current-node (+ level 1)))
;; 	       ((string= (get-attribute current-node "SOURCE"
	      
       
