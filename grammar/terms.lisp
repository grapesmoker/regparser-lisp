(in-package :regulations-parser)

(defparameter +prohibited-terms+
  (append *lowercase-letters* *uppercase-letters* *numerics*
	  '("e.g.," "e.g." "see")))

(defun =tagged-term ()
  (=let* ((_ (=emph-tag-open))
          (term 
           (=one-or-more
	    (=not (=emph-tag-close))))
          (_ (=emph-tag-close))
	  (_ (=skip-whitespace
	      (=string "means"))))
    (=result (list->string term))))

(defun =quoted-term ()
  (=let* ((_ (=or (=character #\")
		  (=character #\LEFT_DOUBLE_QUOTATION_MARK)))
	  (term (=one-or-more
		 (=not (=or (=character #\")
			    (=character #\RIGHT_DOUBLE_QUOTATION_MARK)))))
	  (_ (=or (=character #\")
	   	  (=character #\RIGHT_DOUBLE_QUOTATION_MARK)))
	  (_ (=skip-whitespace
	       (=string "means"))))
    (=result (stringify term))))

(defun =defined-term ()
  (=or (=tagged-term)
       (=quoted-term)))

(defun strip-emph-tags (text)
  (regex-replace-all "<E T=\"[0-9]+\">"
                 (regex-replace-all "</E>" text "")
                 ""))

(defun strip-anchor-tags (text)
  (regex-replace-all "<a[^>]*>"
		 (regex-replace-all "</a>" text "")
		 ""))

(defun =defined-term-in-text (term)
  (=let* ((term
           (=or
            (=string term)
            (=string (string-upcase term))
            (=string (string-upcase term :end 1))
            (=string (string-capitalize term)))))
    (=result term)))

(defun is-stand-alone-term (start end text)
  (unless (< start end)
    (error (format nil "Start position (~D) must be less than end position (~D)!" start end)))
  (cond ((and (= start 0) (< end (length text)))
         (not (alpha-char-p (character (subseq text end (+ 1 end))))))
        ((and (> start 0) (< end (- (length text) 1)))
         (let ((prev-character (character (subseq text (- start 1) start)))
               (next-character (character (subseq text end (+ 1 end)))))
           (and (not (alpha-char-p prev-character))
                (not (alpha-char-p next-character))
                (not (char= #\> prev-character))
                (not (char= #\< next-character))
                (not (char= #\" prev-character next-character)))))
        ((and (> start 0) (= end (- (length text) 1)))
         (not (alpha-char-p (character (subseq text (- start 1) start)))))))

(defun offset-inside-tag (start end tag text)
  (let ((open-tag-positions (all-matches (format nil "<~A" tag) text))
	(close-tag-positions (all-matches (format nil "</~A>" tag) text)))
    (loop
       for open-tag-pos in open-tag-positions by #'cddr
       for close-tag-pos in (rest close-tag-positions) by #'cddr
       if (interval-overlap (list open-tag-pos close-tag-pos)
			    (list start end))
       do (return t)
       finally (return nil))))
