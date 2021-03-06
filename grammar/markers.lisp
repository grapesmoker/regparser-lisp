(in-package :regparser)

(defun =numeric-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more (=digit))
            (=character #\)))))
    (=result (stringify marker-list))))

(defun =interp-numeric-marker ()
  (=let* ((marker-list
           (=list
            (=digit-sequence)
            (=character #\.))))
    (=result (list->string marker-list))))

(defun =alpha-lower-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more
             (=satisfies 
              #'(lambda (ch)
                  (find ch *lowercase-letters* :test #'string=))))
            (=character #\)))))
    (=result (stringify marker-list))))

(defun =interp-alpha-lower-marker ()
  (=let* ((marker-list
           (=list
            (=one-or-more
             (=satisfies 
              #'(lambda (ch)
                  (find ch *lowercase-letters* :test #'string=))))
            (=character #\.))))
    (=result (stringify marker-list))))

(defun =alpha-upper-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more
             (=satisfies 
              #'(lambda (ch)
                  (find ch *uppercase-letters* :test #'string=))))
            (=character #\)))))
    (=result (stringify marker-list))))

(defun =interp-alpha-upper-marker ()
  (=let* ((marker-list
           (=list
            (=one-or-more
             (=satisfies 
              #'(lambda (ch)
                  (find ch *uppercase-letters* :test #'string=))))
            (=character #\.))))
    (=result (stringify marker-list))))

(defun =small-roman-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more
             (=satisfies
              #'(lambda (ch)
                  (find ch *small-romans* :test #'string=))))
            (=character #\)))))
    (=result (stringify marker-list))))

(defun =interp-small-roman-marker ()
  (=let* ((marker-list
           (=list
            (=one-or-more
             (=satisfies
              #'(lambda (ch)
                  (find ch *small-romans* :test #'string=))))
            (=character #\.))))
    (=result (stringify marker-list))))

(defun =big-roman-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more
             (=satisfies
              #'(lambda (ch)
                  (find ch *big-romans* :test #'string=))))
            (=character #\)))))
    (=result (stringify marker-list))))

(defun =interp-big-roman-marker ()
  (=let* ((marker-list
           (=list
            (=one-or-more
             (=satisfies
              #'(lambda (ch)
                  (find ch *big-romans* :test #'string=))))
            (=character #\.))))
    (=result (stringify marker-list))))

(defun =marker ()
  (=or
   (=alpha-lower-marker)
   (=numeric-marker)
   (=small-roman-marker)
   (=alpha-upper-marker)
   (=big-roman-marker)))

(defun =emph-marker-1 ()
  (=let* ((_ (=emph-tag-open))
	  (marker (=marker))
	  (_ (=emph-tag-close)))
    (=result marker)))

(defun =emph-marker-2 ()
  (=let* ((left-paren (=character #\())
	  (_ (=emph-tag-open))
	  (marker (=or (=one-or-more (=digit))
		       (=one-or-more (=satisfies #'lower-case-p))
		       (=one-or-more (=satisfies #'upper-case-p))))
	  (_ (=emph-tag-close))
	  (right-paren (=character #\))))
    (=result (stringify (list left-paren marker right-paren)))))

(defun =emph-marker ()
  (=or (=emph-marker-1)
       (=emph-marker-2)))

(defun =emph-marker-for-cites ()
  (=let* ((left-paren (=character #\())
	  (open-tag (=emph-tag-open))
	  (marker (=or (=one-or-more (=digit))
		       (=one-or-more (=satisfies #'lower-case-p))
		       (=one-or-more (=satisfies #'upper-case-p))))
	  (close-tag (=emph-tag-close))
	  (right-paren (=character #\))))
    (=result (concatenate 'string
			  (stringify left-paren)
			  open-tag
			  (stringify marker)
			  close-tag
			  (stringify right-paren)))))

(defun =interp-marker ()
  (=or
   (=interp-alpha-lower-marker)
   (=interp-numeric-marker)
   (=interp-small-roman-marker)
   (=interp-alpha-upper-marker)
   (=interp-big-roman-marker)))

(defun =emph-interp-marker ()
  (=let* ((_ (=emph-tag-open))
	  (marker (=interp-marker))
	  (_ (=emph-tag-close)))
    (=result marker)))

(defun =all-markers ()
  (=or (=marker)
       (=interp-marker)
       (=emph-marker)
       (=emph-interp-marker)))

(defun =next-marker ()
  (=let* ((_
           (=zero-or-more
            (=not (=marker))))
          (markers
           (=zero-or-more
            (=marker))))
    (=result markers)))

(defun =marked-paragraph ()
  (=let* ((marker (=marker))
          (rest-of-par
           (=zero-or-more (=sentence))))
    (=result (list (cons 'MARKER marker)
                   (cons 'REST (stringify (flatten (append rest-of-par))))))))

(defun =double-marked-paragraph ()
  (=let* ((marker (=list (=marker)
                         (=marker)))
          (rest-of-par
           (=zero-or-more (=sentence))))
    (=result (list (cons 'MARKER marker)
                   (cons 'REST (flatten (append rest-of-par)))))))

(defun =subpart ()
  (=let* ((_
	   (=or (=string "subpart")
		(=string "Subpart")))
	  (subpart-letter
	   (=skip-whitespace
	    (=satisfies #'(lambda (ch)
			    (find ch *uppercase-letters* :test #'string=))))))
    (=result (stringify subpart-letter))))
