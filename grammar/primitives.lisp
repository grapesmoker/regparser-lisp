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

(defun =one-of-words (list-of-words)
  (=let* ((word (=word)))
    (if (find word list-of-words :test #'string=)
        (=result word)
        (=fail))))

(defun =non-period-punc ()
  (=string-of
   (=one-of '(#\! #\? #\, #\: #\;))))

(defun =punctuation ()
  (=string-of
   (=one-of '(#\. #\! #\? #\, #\: #\;))))

(defun =tag-delimiter ()
  (=string-of
   (=one-of '(#\< #\>))))

(defun =misc-char ()
  (=string-of
   (=one-of '(#\= #\" #\/ #\-))))

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
    (=result (format nil "~{~A~}" (stringify sentence-list)))))

(defun =appendix ()
  (=let* ((appendix (=or (=string "Appendix")
                         (=string "appendix")))
          (letter (=skip-whitespace
                   (=one-or-more (=or (=satisfies #'upper-case-p)
                                      (=character #\-)
                                      (=digit))))))
    (=result (cons 'APPENDIX (stringify letter)))))

