(in-package :regparser)

(defparameter *lowercase-letters*
  (mapcar 
   #'string
   (mapcar #'code-char (loop for i from 97 to 122 collect i))))

(defparameter *uppercase-letters*
  (mapcar 
   #'string
   (mapcar #'code-char (loop for i from 65 to 90 collect i))))

(defparameter *big-romans*
  (mapcar #'(lambda (ch)
              (format nil "~@r" ch))
          (loop for i from 1 to 200 collect i)))

(defparameter *small-romans*
  (mapcar #'string-downcase *big-romans*))

(defparameter *numerics*
  (mapcar 'write-to-string (loop for i from 1 to 100 collect i)))

(defparameter *foo* 'test)

(defun =word ()
  (=let* ((word (=one-or-more (=satisfies #'alpha-char-p))))
    (=result (concatenate 'string word))))

(defun =non-whitespace-token ()
  (=skip-whitespace
   (=zero-or-more
    (=not (=whitespace)))))

(defun =one-of-words (list-of-words)
  (=let* ((word (=word)))
    (if (find word list-of-words :test #'string=)
        (=result word)
        (=fail))))

(defun =non-period-punc ()
  (=string-of
   (=one-of '(#\! #\? #\, #\: #\; #\' #\" #\( #\) #\- #\SECTION_SIGN
              #\LEFT_DOUBLE_QUOTATION_MARK #\RIGHT_DOUBLE_QUOTATION_MARK))))

(defun =punctuation ()
  (=string-of
   (=one-of '(#\. #\! #\? #\, #\: #\;))))

(defun =tag-delimiter ()
  (=string-of
   (=one-of '(#\< #\>))))

(defun =misc-char ()
  (=string-of
   (=one-of '(#\= #\" #\/ #\- #\SECTION_SIGN
              #\LEFT_DOUBLE_QUOTATION_MARK #\RIGHT_DOUBLE_QUOTATION_MARK
              #\' #\( #\)))))

(defun =parens ()
  (=string-of
   (=one-of '(#\( #\)))))

(defun =digit-sequence ()
  (=let* ((result
           (=one-or-more (=digit))))
    (=result (stringify result))))

(defun =sentence ()
  (=let* ((sentence-list 
           (=one-or-more
            (=or
             (=tag-delimiter)
             (=parens)
             (=misc-char)
             (=string-of (=whitespace))
             (=string-of (=digit))
             (=one-or-more (=word))
             (=punctuation)))))
    (=result (list->string (stringify sentence-list)))))

(defun =appendix ()
  (=let* ((appendix (=or (=string "Appendix")
                         (=string "appendix")))
          (letter (=skip-whitespace
                   (=one-or-more (=or (=satisfies #'upper-case-p)
                                      (=character #\-)
                                      (=digit))))))
    (=result (cons 'APPENDIX (stringify letter)))))

(defun =section ()
  (=let* ((_
           (=or
            (=string "Section")
            (=string "section")
            (=character #\SECTION_SIGN)))
          (part-number
           (=skip-whitespace
            (=digit-sequence)))
          (_ (=character #\.))
          (section-number
           (=digit-sequence)))
    (=result (list (cons :part-number part-number)
                   (cons :section-number section-number)))))

(defun =emph-tag-open ()
  (=let* ((result
           (=list 
            (=string "<E T=\"")
            (=digit-sequence)
            (=string "\">"))))
    (=result (list->string result))))

(defun =emph-tag-close ()
  (=string "</E>"))

(defun =emph-text ()
  (=let* ((result
           (=list
            (=emph-tag-open)
            (=one-or-more
             (=or
              (=word)
              (=string-of (=whitespace))))
            (=emph-tag-close))))
    (=result (list (cons 'OPEN-EMPH (first result))
                   (cons 'CONTENTS (list->string (second result)))
                   (cons 'CLOSE-EMPH (last result))))))

(defun =intro ()
  (=string "Introduction"))
