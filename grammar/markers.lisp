(in-package :regparser)

(defun =numeric-marker ()
  (=let* ((marker-list
           (=list
            (=character #\()
            (=one-or-more (=digit))
            (=character #\)))))
    (=result (stringify marker-list))))

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

(defun =marker ()
  (=or
   (=alpha-lower-marker)
   (=numeric-marker)
   (=small-roman-marker)
   (=alpha-upper-marker)
   (=big-roman-marker)))

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
    (print rest-of-par)
    (=result (list (cons 'MARKER marker)
                   (cons 'REST (flatten (append rest-of-par)))))))
