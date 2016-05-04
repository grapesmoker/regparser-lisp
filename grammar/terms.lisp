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
