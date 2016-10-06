(in-package :regparser)

(defstruct paragraph
  (marker "" :type string)
  (title "" :type string)
  (label "" :type string)
  (content "" :type string)
  (depth -1 :type integer)
  (subparagraphs '() :type list))

(defun strip-par-tags (par-text)
  (regex-replace "</P>"
                 (regex-replace "<P>" par-text "")
                 ""))

(defun strip-prtpage-tag (par-text)
  (regex-replace "<PRTPAGE P=\"[0-9]+\"/>" par-text " "))

(defun build-paragraph (p-elem &key (par-mode :regtext))
  (declare (optimize (debug 3)))
  (strip p-elem)
  (let* ((p-elem-text (strip-par-tags (serialize p-elem nil)))
	 (make-par-func
	  (case par-mode
	    (:regtext
	     #'make-paragraph)
	    (:interp
	     #'make-interp-paragraph)))
	 (parsed-par (run 
		      (case par-mode
			(:regtext
			 (=paragraph))
			(:interp 
			 (=interp-paragraph)))
		      p-elem-text))
	 (first-marker (cdr (assoc 'first-marker parsed-par)))
	 (title (cdr (assoc 'title parsed-par)))
	 (title-content
	  (if title
	      (cdr (assoc 'content title))
	      ""))
	 (second-marker (cdr (assoc 'second-marker parsed-par)))
	 (second-title (cdr (assoc 'second-title parsed-par)))
	 (second-title-content
	  (if second-title
	      (cdr (assoc 'content second-title))
	      ""))
	 (raw-content (cadr (assoc 'content parsed-par)))
	 (content (strip-prtpage-tag raw-content)))
    ;; The first, most specific option is a "paragraph" that is
    ;; actually two paragraphs, each with a marker and a title,
    ;; with the content actually belonging to the second paragraph
    (cond ((and first-marker title second-marker second-title)
	   (let ((first-paragraph
		  (funcall make-par-func
			   :marker first-marker
			   :title title-content))
		 (second-paragraph
		  (funcall make-par-func
			   :marker second-marker
			   :title second-title-content
			   :content content)))
	     ;; (format t "option 1~%")
	     (list first-paragraph second-paragraph)))
	  ;; This is the same sort of situation except there's
	  ;; no second title
	  ((and first-marker title second-marker)
	   (let ((first-paragraph
		  (funcall make-par-func 
			   :marker first-marker
			   :title title-content))
		 (second-paragraph
		  (funcall make-par-func
			   :marker second-marker
			   :content content)))
	     ;; (format t "option 2~%")
	     (list first-paragraph second-paragraph)))
	  ;; In this case there is no first marker, but there is
	  ;; a second marker and second title, so this is actually
	  ;; two paragraphs, an unmarked one followed by a marked one
	  ((and title second-marker second-title)
	   (let ((first-paragraph
		  (funcall make-par-func :title title-content))
		 (second-paragraph
		  (funcall make-par-func
			   :title second-title-content
			   :marker second-marker
			   :content content)))
	     (list first-paragraph second-paragraph)))
	  ;; In this case there is a first marker and a title
	  ;; but there's no second paragraph
	  ((and first-marker title)
	   (let ((paragraph (funcall make-par-func
				     :marker first-marker
				     :title title-content
				     :content content)))
	     ;; (format t "option 3~%")
	     (list paragraph)))
	  ;; In this case there's a first marker but no title
	  (first-marker
	   (let ((paragraph (funcall make-par-func
				     :marker first-marker
				     :content content)))
	     ;; (format t "option 4~%")
	     (list paragraph)))
	  ;; An unmarked paragraph that has nothing but a title
	  (title
	   (let ((paragraph (funcall make-par-func
				     :title title-content)))
	     (list paragraph)))
	  ;; This is an unmarked paragraph with some content
	  (content
	   (let ((paragraph (funcall make-par-func :content content)))
	     ;; (format t "option 5~%")
	     (list paragraph))))))

;; (defun =paragraph-title ()
;;   (=let* ((result
;;            (=list
;;             (=emph-tag-open)
;;             (=one-or-more
;;              (=or
;;               (=word)
;;               (=string-of (=whitespace))
;;               (=non-period-punc)))
;;             (=string-of (=one-of '(#\. #\" #\RIGHT_DOUBLE_QUOTATION_MARK)))
;;             (=maybe (=character #\—))
;;             (=emph-tag-close))))
;;     (=result (list (cons 'OPEN-EMPH (first result))
;;                    (cons 'CONTENT (list->string (concatenate 'list
;;                                                              (second result)
;;                                                              (third result))))
;;                    (cons 'CLOSE-EMPH (fifth result))))))

(defun =paragraph-title ()
  (=let* ((_ (=emph-tag-open))
	  (title
	   (=one-or-more
	    (=not (=emph-tag-close))))
	  (_ (=emph-tag-close)))
    (=result (list (cons 'content (stringify title))))))

(defun =paragraph ()
  (=let* ((result
           (=list
            (=skip-whitespace (=maybe (=marker)))
            (=skip-whitespace (=maybe (=paragraph-title)))
            (=skip-whitespace (=maybe (=marker)))
            (=skip-whitespace (=maybe (=paragraph-title)))
            (=one-or-more
             (=sentence)))))
    (=result (list (cons 'FIRST-MARKER (elt result 0))
                   (cons 'TITLE (elt result 1))
                   (cons 'SECOND-MARKER (elt result 2))
                   (cons 'SECOND-TITLE (elt result 3))
                   (cons 'CONTENT (elt result 4))))))

(defun =emph-tag ()
  (=let* ((result
           (=list (=string "<E T=\"03\">")
                  (=one-or-more
                   (=or
                    (=word)
                    (=string-of (=whitespace))
                    (=punctuation)))
                  (=string "</E>"))))
    (=result (list (cons 'OPEN-TAG (first result))
                   (cons 'CONTENTS (list->string (second result)))
                   (cons 'CLOSE-TAG (last result))))))
    
(defparameter *test-paragraph-1*
  (aref 
   (children (parse "<P>(a)<E T=\"03\">Authority.</E>This part, known as Regulation C, is issued by the Bureau of Consumer Financial Protection (Bureau) pursuant to the Home Mortgage Disclosure Act (HMDA) (12 U.S.C. 2801<E T=\"03\">et seq.</E>), as amended. The information-collection requirements have been approved by the U.S. Office of Management and Budget (OMB) under 44 U.S.C. 3501 <E T=\"03\">et seq.</E> and have been assigned OMB numbers for institutions reporting data to the Office of the Comptroller of the Currency (1557-0159), the Federal Deposit Insurance Corporation (3064-0046), the Federal Reserve System (7100-0247), the Department of Housing and Urban Development (HUD) (2502-0529), the National Credit Union Administration (3133-0166), and the Bureau of Consumer Financial Protection (3170-0008).</P>")) 0))

(defparameter *test-paragraph-2*
  (aref
   (children (parse "<P>(b)<E T=\"03\">Purpose.</E>(1) This part implements the Home Mortgage Disclosure Act, which is intended to provide the public with loan data that can be used:</P>")) 0))

(defparameter *test-paragraph-3*
  (aref
   (children (parse "<P>(i) To help determine whether financial institutions are serving the housing needs of their communities;</P>")) 0))

(defparameter *test-paragraph-4*
  (aref
   (children (parse "<P><E T=\"03\">Application.&#x2014;</E>(1)<E T=\"03\">In general.</E>Application means an oral or written request for a home purchase loan, a home improvement loan, or a refinancing that is made in accordance with procedures used by a financial institution for the type of credit requested.</P>")) 0))

(defparameter *test-paragraph-5*
  (aref
   (children (parse "<P><E T=\"03\">Act</E>means the Home Mortgage Disclosure Act (HMDA) (12 U.S.C. 2801<E T=\"03\">et seq.</E>), as amended.</P>")) 0))

(defparameter *test-paragraph-6*
  (nth-child 0 (parse "<P>1.<E T=\"03\">General.</E>The comments in this section address issues affecting coverage of institutions and exemptions from coverage.</P>")))

(defparameter *text-1* 
  (strip-par-tags (serialize *test-paragraph-1* nil)))

(defparameter *text-2*
  (strip-par-tags (serialize *test-paragraph-2* nil)))

(defparameter *text-3*
  (strip-par-tags (serialize *test-paragraph-3* nil)))

(defparameter *text-4*
  (strip-par-tags (serialize *test-paragraph-4* nil)))

(defparameter *text-5*
  (strip-par-tags (serialize *test-paragraph-5* nil)))

(defun build-defined-terms (text par-label)
  (let* ((defined-terms (extract-all-tokens #'=defined-term text))
         (tokens 
          (mapcar #'(lambda (def-term)
                      (strip-emph-tags (cdr (assoc :token def-term))))
                  defined-terms))
         (start-end-list
          (mapcar #'(lambda (def-term)
                      (let ((start (cdr (assoc :start def-term)))
                            (end (cdr (assoc :end def-term))))
                        (list start end)))
                  defined-terms))
         (terms-xml
          (mapcar #'(lambda (term)
                      (format nil "<def term=~S>~A</def>" (string-downcase term) term))
                  tokens))
         (interpolated-terms
          (if start-end-list
              (insert-into-locations text terms-xml start-end-list)
              text)))
    ;; (print tokens)
    ;; (print start-end-list)
    (loop
       for term in tokens
       do
         (setf (gethash (string-downcase term) +defined-terms+) par-label))
    interpolated-terms))

(defun build-term-references (text)
  (loop
     for term being the hash-keys of +defined-terms+
     using (hash-value target)
     with running-text = text
     do
       (let* ((defined-terms (extract-all-tokens #'(lambda ()
                                                     (=defined-term-in-text term))
                                                 running-text)))
         (loop
            for def-term in defined-terms
            with terms-xml = '()
            with start-end-list = '()
            do
              (let* (;;(token (cdr (assoc :token def-term)))
                     (start (cdr (assoc :start def-term)))
                     (end (cdr (assoc :end def-term)))
                     (standalone (is-stand-alone-term start end running-text))
                     (target (gethash (string-downcase term) +defined-terms+))
                     (term-xml
                      (when standalone
                        (format nil "<ref target=~S reftype=\"term\">~A</ref>" 
                                target (subseq running-text start end)))))
                (when standalone
                  (push term-xml terms-xml)
                  (push (list start end) start-end-list)))
            finally
              (unless (null terms-xml)
                (let ((new-text 
                       (insert-into-locations running-text
                                              (reverse terms-xml)
                                              (reverse start-end-list))))
                  (setf running-text new-text)))))
     finally
       (return running-text)))
                     
(defun build-internal-citations (part-number section-number text)
  (let ((citations (extract-all-tokens #'=internal-citation text)))
    (loop
       for cite in citations
       with citations-xml = '()
       with start-end-list = '()
       do
         (let* ((token (cdr (assoc :token cite)))
                (start (cdr (assoc :start cite)))
                (end (cdr (assoc :end cite)))
                (citation-xml
                 (format nil "<ref target=~S reftype=\"internal\">~A</ref>"
                         (run (=internal-citation->label part-number section-number) token) token)))
           (push citation-xml citations-xml)
           (push (list start end) start-end-list))
       finally
         (if citations-xml
             (return (insert-into-locations text (reverse citations-xml) (reverse start-end-list)))
             (return text)))))

(defun find-and-interpolate-definitions (paragraphs)
  (loop
     for par in paragraphs
     do
       (let ((par-text-with-defs
              (build-defined-terms (paragraph-content par) (paragraph-label par))))
         (setf (paragraph-content par) par-text-with-defs)
         (find-and-interpolate-definitions (paragraph-subparagraphs par)))))

(defun scan-and-interpolate-termrefs-and-cites (paragraphs part-number section-number)
  (loop
     for par in paragraphs
     do
       (let ((par-text-with-refs
              (build-internal-citations part-number section-number
               (build-term-references (paragraph-content par)))))
         (setf (paragraph-content par) par-text-with-refs)
         (scan-and-interpolate-termrefs-and-cites 
          (paragraph-subparagraphs par) part-number section-number))))

;;(defun scan-and-interpolate-internal-cites (paragraphs part-number section-number)
;;  (loop
;;     for par in paragraphs
           

;; (defun paragraph->xml (par root label-root)
;;   (let* ((marker (paragraph-marker par))
;;          (par-label (format nil "~A-~A" label-root (strip-marker marker)))
;;          (content (format nil "<content>~A</content>" (paragraph-content par)))
;;          (title (paragraph-title par))
;;          (subparagraphs (paragraph-subparagraphs par))
;;          (par-elem
;;           (make-element root "paragraph")))
;;     (set-attribute par-elem "label" par-label)
;;     (set-attribute par-elem "marker" marker)
;;     (when (not (string= "" title))
;;       (make-fulltext-element par-elem "title" :text title))
;;     ;; (make-fulltext-element par-elem "content" :text content)
;;     (parse content :root par-elem)
;;     (loop
;;        for par in subparagraphs
;;        do
;;          (paragraph->xml par par-elem par-label))))

(defun paragraph->xml (paragraph &optional (depth 0) (indent 4))
    (let* ((par-start-tag
	    (format nil "~A<paragraph marker=\"~A\" label=\"~A\">~%"
		      (indent (* depth indent))
		      (paragraph-marker paragraph)
		      (paragraph-label paragraph)))
	   (par-end-tag (format nil "~A</paragraph>~%" (indent (* depth indent))))
	   (title
	    (if (not (string= "" (paragraph-title paragraph)))
		(format nil "~A<title>~A</title>~%"
			(indent (* (+ depth 1) indent))
			(paragraph-title paragraph))
		""))
	   (content
	    (format nil "~A<content>~A</content>~%"
		    (indent (* (+ depth 1) indent))
		    (paragraph-content paragraph)))
	   (subparagraphs
	    (loop
	       for subpar in (paragraph-subparagraphs paragraph)
	       collect
		 (paragraph->xml subpar (+ 1 depth)))))
      (format nil "~A~A~A~{~A~}~A" par-start-tag title content subparagraphs par-end-tag)))
