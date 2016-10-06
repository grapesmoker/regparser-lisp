(in-package :regulations-parser)

(defun =interp-header (part-number)
  (=let* ((header-prefix
           (=string "Supplement I to Part"))
          (space
           (=one-or-more (=whitespace)))
          (part (=string part-number)))
    (=result (format nil "~A~A~A" header-prefix (list->string space) part))))

(defun =interp-section-header (part-number)
  (=let* ((section-prefix (=string "Section"))
          (space (=zero-or-more (=whitespace)))
          (part (=string part-number))
          (period (=string-of (=character #\.)))
          (section (=digit-sequence)))
    ;; (=result (format nil "~A~A~D~A~D" 
    ;;                 section-prefix (list->string space) part period
    ;;                 section))))
    (=result (list (cons :part part)
                   (cons :section section)))))
                     
(defun =interp-paragraph-header ()
  (=let* ((paragraph
           (=maybe (=string "Paragraph")))
          (space
           (=zero-or-more (=whitespace)))
          (section (=digit-sequence))
          (markers (=one-or-more (=marker)))
          (rest (=zero-or-more (=item))))
    ;; (=result 
    ;;  (if paragraph
    ;;    (format nil "~A~A~A~{~A~}" paragraph (list->string space) section markers)
    ;;    (format nil "~A~{~A~}" section markers)))))
    (=result
     (list (cons :section section)
           (cons :markers markers)
           (cons :full-text
                 (if paragraph
                     (format nil "~A~A~A~{~A~} ~A" 
                             paragraph (list->string space) section markers
                             (list->string rest))
                     (format nil "~A~{~A~} ~A" section markers (list->string rest))))))))

(defun =interp-subpart ()
  (=let* ((_ (=or (=string "subpart")
		  (=string "Subpart")))
	  (subpart-letter
	   (=skip-whitespace
	    (=satisfies #'(lambda (ch)
			    (find ch *uppercase-letters* :test #'string=))))))
    (=result subpart-letter)))
	   
