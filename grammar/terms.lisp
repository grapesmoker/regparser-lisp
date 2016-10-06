(in-package :regulations-parser)

(defun =defined-term ()
  (=let* ((emph-open (=emph-tag-open))
          (term 
           (=one-or-more
            (=or
             (=word)
             (=string-of (=whitespace))
             (=non-period-punc))))
          (emph-close (=emph-tag-close)))
    (=result (list->string (list emph-open (list->string term) emph-close)))))

(defun strip-emph-tags (text)
  (regex-replace "<E T=\"[0-9]+\">"
                 (regex-replace "</E>" text "")
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
