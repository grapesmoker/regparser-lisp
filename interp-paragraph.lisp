(in-package :regulations-parser)

(defstruct interp-paragraph
  (marker "" :type string)
  (title "" :type string)
  (label "" :type string)
  (target "" :type string)
  (content "" :type string)
  (depth -1 :type integer)
  (subparagraphs '() :type list))

;; (defun copy-interp-paragraph (ipar)
;;   (make-interp-paragraph
;;    :marker (interp-paragraph-marker ipar)
;;    :title (interp-paragraph-title ipar)
;;    :label (interp-paragraph-label ipar)
;;    :target (interp-paragraph-target ipar)
;;    :content (interp-paragraph-content ipar)
;;    :depth (interp-paragraph-depth ipar)
;;    :subparagraphs
;;    (loop
;;       for subpar in (interp-paragraph-subparagraphs ipar)
;;       collect
;; 	(copy-interp-paragraph subpar))))

(defun =interp-paragraph ()
  (=let* ((result
           (=list
            (=skip-whitespace (=maybe (=interp-marker)))
            (=skip-whitespace (=maybe (=paragraph-title)))
            (=skip-whitespace (=maybe (=interp-marker)))
            (=skip-whitespace (=maybe (=paragraph-title)))
            (=zero-or-more
             (=sentence)))))
    (=result (list (cons 'FIRST-MARKER (elt result 0))
                   (cons 'TITLE (elt result 1))
                   (cons 'SECOND-MARKER (elt result 2))
                   (cons 'SECOND-TITLE (elt result 3))
                   (cons 'CONTENT (elt result 4))))))

(defun interp-paragraph->xml (interp-paragraph &optional (depth 0) (indent 4))
  (let* ((par-start-tag
	  (if (not (string= "" (interp-paragraph-target interp-paragraph)))
	      (format nil "~A<interpParagraph marker=\"~A\" label=\"~A\" target=\"~A\">~%"
		      (indent (* depth indent))
		      (interp-paragraph-marker interp-paragraph)
		      (interp-paragraph-label interp-paragraph)
		      (interp-paragraph-target interp-paragraph))
	      (format nil "~A<interpParagraph marker=\"~A\" label=\"~A\">~%"
		      (indent (* depth indent))
		      (interp-paragraph-marker interp-paragraph)
		      (interp-paragraph-label interp-paragraph))))
	 (par-end-tag (format nil "~A</interpParagraph>~%" (indent (* depth indent))))
	 (title
	  (if (not (string= "" (interp-paragraph-title interp-paragraph)))
	      (format nil "~A<title>~A</title>~%"
		      (indent (* (+ depth 1) indent))
		      (interp-paragraph-title interp-paragraph))
	      ""))
	 (content
	  (format nil "~A<content>~A</content>~%"
		  (indent (* (+ depth 1) indent))
		  (interp-paragraph-content interp-paragraph)))
	 (subparagraphs
	  (loop
	     for subpar in (interp-paragraph-subparagraphs interp-paragraph)
	     collect
	       (interp-paragraph->xml subpar (+ 1 depth)))))
    
    (format nil "~A~A~A~{~A~}~A" par-start-tag title content subparagraphs par-end-tag)))
