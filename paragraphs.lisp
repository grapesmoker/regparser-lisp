(in-package :regparser)

(defstruct paragraph
  (marker "" :type string)
  (title "" :type string)
  (content "" :type string)
  (depth -1 :type integer)
  (subparagraphs '() :type list))

(defun strip-par-tags (par-text)
  (regex-replace "</P>"
                 (regex-replace "<P>" par-text "")
                 ""))

(defun build-paragraph (p-elem)
  (strip p-elem)
  (let* ((p-elem-text (strip-par-tags (serialize p-elem nil)))
         (parsed-par (run (=paragraph) p-elem-text))
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
         (content (cadr (assoc 'content parsed-par))))
    ;; The first, most specific option is a "paragraph" that is
    ;; actually two paragraphs, each with a marker and a title,
    ;; with the content actually belonging to the second paragraph
    (cond ((and first-marker title second-marker second-title)
           (let ((first-paragraph
                  (make-paragraph :marker first-marker
                                  :title title-content))
                 (second-paragraph
                  (make-paragraph :marker second-marker
                                  :title second-title-content
                                  :content content)))
             ;; (format t "option 1~%")
             (list first-paragraph second-paragraph)))
          ;; This is the same sort of situation except there's
          ;; no second title
          ((and first-marker title second-marker)
           (let ((first-paragraph
                  (make-paragraph :marker first-marker
                                  :title title-content))
                 (second-paragraph
                  (make-paragraph :marker second-marker
                                  :content content)))
             ;; (format t "option 2~%")
             (list first-paragraph second-paragraph)))
          ;; In this case there is no first marker, but there is
          ;; a second marker and second title, so this is actually
          ;; two paragraphs, an unmarked one followed by a marked one
          ((and title second-marker second-title)
           (let ((first-paragraph
                  (make-paragraph :title title-content))
                 (second-paragraph
                  (make-paragraph :title second-title-content
                                  :marker second-marker
                                  :content content)))
             (list first-paragraph second-paragraph)))
          ;; In this case there is a first marker and a title
          ;; but there's no second paragraph
          ((and first-marker title)
           (let ((paragraph (make-paragraph :marker first-marker
                                            :title title-content
                                            :content content)))
             ;; (format t "option 3~%")
             (list paragraph)))
          ;; In this case there's a first marker but no title
          (first-marker
           (let ((paragraph (make-paragraph :marker first-marker
                                            :content content)))
             ;; (format t "option 4~%")
             (list paragraph)))
          ;; This is an unmarked paragraph with some content
          (content
           (let ((paragraph (make-paragraph :content content)))
             ;; (format t "option 5~%")
             (list paragraph))))))

(defun =paragraph-title ()
  (=let* ((result
           (=list
            (=emph-tag-open)
            (=one-or-more
             (=or
              (=word)
              (=string-of (=whitespace))
              (=non-period-punc)))
            (=string-of (=character #\.))
            (=maybe (=character #\—))
            (=emph-tag-close))))
    (=result (list (cons 'OPEN-EMPH (first result))
                   (cons 'CONTENT (list->string (concatenate 'list
                                                             (second result)
                                                             (third result))))
                   (cons 'CLOSE-EMPH (fifth result))))))

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

;; (defun build-defined-terms (text)
;;   (let ((defined-terms (extract-all-tokens #'defined-term text)))
;;     (loop
;;        with running-content = text
;;        for def in defined-terms
;;        do
;;          (let* ((term (strip-emph-tags (cdr (assoc :token def))))
;;                 (start (cdr (assoc :start def)))
;;                 (end (cdr (assoc :end def)))
;;                 (content (split-at-locations text start end))
;;                 (term-xml (format nil "<def term=~S>~S</def>" 
;;                                   (string-downcase term)
;;                                   term)))
;;            (
           
(defun split-at-locations (text start-end-list)
  (loop
     for start-end-pair in start-end-list
     with current-start = 0
     ;; with current-end = (first (first start-end-list))
     collect
       (let* ((start (first start-end-pair))
              (end (second start-end-pair))
              (current-end start)
              (piece (subseq text current-start current-end)))
         (setf current-start end)
         piece)
     into result
     finally
       (return (append result (list (subseq text (second start-end-pair)))))))

(defun insert-into-locations (text values start-end-list)
  (let* ((split-text (split-at-locations text start-end-list)))
    (list->string
     (loop
	for val in values
	for piece in split-text
	append
	  (list piece val)
	into result
	finally
	  (return (append result (last split-text)))))))

(defun paragraph->xml (par root label-root)
  (let* ((marker (paragraph-marker par))
         (par-label (format nil "~A-~A" label-root (strip-marker marker)))
         (content (paragraph-content par))
         (title (paragraph-title par))
         (subparagraphs (paragraph-subparagraphs par))
         (par-elem
          (make-element root "paragraph")))
    (set-attribute par-elem "label" par-label)
    (set-attribute par-elem "marker" marker)
    (when (not (string= "" title))
      (make-fulltext-element par-elem "title" :text title))
    (make-fulltext-element par-elem "content" :text content)
    (loop
       for par in subparagraphs
       do
         (paragraph->xml par par-elem par-label))))
