(in-package :regparser)

(defstruct section
  (section-number "" :type string)
  (part-number "" :type string)
  (subject "" :type string)
  (label "" :type string)
  (paragraphs '() :type list))

(defun build-section (section-elem label-root)
  (let* ((sec-num-elem (first (get-elements-by-tag-name section-elem "SECTNO")))
         (subject-elem (first (get-elements-by-tag-name section-elem "SUBJECT")))
         (section-info (run (=section) (text sec-num-elem)))
         (section-number
          (cdr (assoc :section-number section-info)))
         (part-number
          (cdr (assoc :part-number section-info)))
         (subject (text subject-elem))
         (label (format nil "~A-~D" label-root section-number))
         (paragraphs
          (flatten
           (loop
              for child across (children section-elem)
              collect
                (when (and (element-p child)
                           (string= "P" (tag-name child)))
                  (build-paragraph child)))))
         (markers
          (mapcar #'(lambda (par)
		      (strip-marker (paragraph-marker par)))
                      ;;(regex-replace #\(
                      ;;               (regex-replace #\) (paragraph-marker par) "") ""))
                  paragraphs))
         (hierarchy
          (compute-marker-hierarchy markers))
         (paragraph-tree
          (build-paragraph-tree paragraphs hierarchy label)))
    (make-section :section-number section-number
                  :part-number part-number
                  :subject subject
                  :label label
                  :paragraphs paragraph-tree)))
    

(defun build-paragraph-tree (paragraphs hierarchy label-root)
  (loop
     with last-par-at-depth = (make-hash-table)
     with top-level-pars = '()
     for par in (rest paragraphs)
     for marker in (rest hierarchy)
     initially
       (setf (paragraph-depth (first paragraphs)) (marker-depth (first hierarchy)))
       (push (first paragraphs) top-level-pars)
       (setf (gethash (marker-depth (first hierarchy)) last-par-at-depth) (first paragraphs))
     do
       (let* ((depth (marker-depth marker))
              (last-par-at-this-depth (gethash depth last-par-at-depth)))
         (setf (paragraph-depth par) depth)
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
                  (push par (paragraph-subparagraphs last-par-at-prev-depth))
                  (setf (gethash depth last-par-at-depth) par)))
               ((null last-par-at-this-depth)
                (let ((last-par-at-prev-depth (gethash (- depth 1) last-par-at-depth)))
                  ;; (format t "no par at depth ~A~%" depth)
                  (push par (paragraph-subparagraphs last-par-at-prev-depth))
                  (setf (gethash depth last-par-at-depth) par)))))
     finally
       (labels ((recursive-reverse-and-label (list-of-pars par-label-root)
                  (loop
                     for par in list-of-pars
                     for index downfrom (length list-of-pars)
                     do
                       (let ((par-label
                              (if (not (string= (paragraph-marker par) ""))
                                  (format nil "~A-~A" par-label-root (strip-marker (paragraph-marker par)))
                                  (format nil "~A-p~D" par-label-root index))))
                         (setf (paragraph-label par) par-label)
                         (setf (paragraph-subparagraphs par) (reverse (paragraph-subparagraphs par)))
                         (recursive-reverse-and-label (paragraph-subparagraphs par) par-label)))))
         (recursive-reverse-and-label top-level-pars label-root)
         (return (reverse top-level-pars)))))

(defparameter *test-section-1*
  (strip 
   (aref
    (children
     (parse "<SECTION>
              <SECTNO>&#xA7; 1003.1</SECTNO>
              <SUBJECT>Authority, purpose, and scope.</SUBJECT>
              <P>(a)<E T=\"03\">Authority.</E>This part, known as Regulation C, is issued by the Bureau of Consumer Financial Protection (Bureau) pursuant to the Home Mortgage Disclosure Act (HMDA) (12 U.S.C. 2801<E T=\"03\">et seq.</E>), as amended. The information-collection requirements have been approved by the U.S. Office of Management and Budget (OMB) under 44 U.S.C. 3501<E T=\"03\">et seq.</E>and have been assigned OMB numbers for institutions reporting data to the Office of the Comptroller of the Currency (1557-0159), the Federal Deposit Insurance Corporation (3064-0046), the Federal Reserve System (7100-0247), the Department of Housing and Urban Development (HUD) (2502-0529), the National Credit Union Administration (3133-0166), and the Bureau of Consumer Financial Protection (3170-0008).</P>
              <P>(b)<E T=\"03\">Purpose.</E>(1) This part implements the Home Mortgage Disclosure Act, which is intended to provide the public with loan data that can be used:</P>
              <P>(i) To help determine whether financial institutions are serving the housing needs of their communities;</P>
              <P>(ii) To assist public officials in distributing public-sector investment so as to attract private investment to areas where it is needed; and</P>
              <P>(iii) To assist in identifying possible discriminatory lending patterns and enforcing antidiscrimination statutes.</P>
              <P>(2) Neither the act nor this part is intended to encourage unsound lending practices or the allocation of credit.</P>
              <P>(c)<E T=\"03\">Scope.</E>This part applies to certain financial institutions, including banks, savings associations, credit unions, and other mortgage lending institutions, as defined in &#xA7; 1003.2. The regulation requires an institution to report data to the appropriate Federal agency about home purchase loans, home improvement loans, and refinancings that it originates or purchases, or for which it receives applications; and to disclose certain data to the public.</P>
            </SECTION>")) 0)))

;; (defun section->xml (section root label-root)
;;   (let* ((section-number (section-section-number section))
;;          (part-number (section-part-number section))
;;          (subject (section-subject section))
;;          (label (format nil "~A-~A" label-root section-number))
;;          (paragraphs (section-paragraphs section))
;;          (section-elem
;;           (make-element root "section")))
;;     (set-attribute section-elem "label" label)
;;     (set-attribute section-elem "sectionNum" section-number)
;;     (make-fulltext-element section-elem "subject" 
;;                            :text (format nil "~C ~D.~D ~A" #\section_sign part-number section-number subject))
;;     (loop
;;        for par in paragraphs
;;        do
;;          (paragraph->xml par section-elem label))))


(defun section->xml (section &optional (depth 0) (indent 4))
  (let* ((label (section-label section))
	 (paragraphs (section-paragraphs section))
	 (section-start-tag
	  (format nil "~A<section label=\"~A\">~%" (indent (* depth indent)) label))
	 (title
	  (format nil "~A<title>~A</title>~%"
		  (indent (* (+ depth 1) indent))
		  (section-subject section)))
	 (section-end-tag (format nil "</section>~%"))
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
