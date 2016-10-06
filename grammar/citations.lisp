(in-package :regulations-parser)

(defun =internal-cite-1 ()
  (=let* ((section 
           (=or
            (=string "section")
            (=string "Section")
            (=string-of (=character #\section_sign))))
          (space
           (=zero-or-more (=whitespace)))
          (part-number (=digit-sequence))
          (period (=character #\.))
          (section-number (=digit-sequence))
          (markers (=zero-or-more (=marker))))
    (=result (format nil "~A~A~D~C~D~{~A~}" 
                     section (list->string space)
                     part-number period section-number markers))))

(defun =internal-cite-1->label ()
  (=let* ((_
           (=or
            (=string "section")
            (=string "Section")
            (=string-of (=character #\section_sign))))
          (_
           (=zero-or-more (=whitespace)))
          (part-number (=digit-sequence))
          (_ (=character #\.))
          (section-number (=digit-sequence))
          (markers (=zero-or-more (=marker))))
    (=result 
     (if markers
         (format nil "~D-~D-~{~A~^-~}" part-number section-number 
                 (mapcar #'strip-marker markers))
         (format nil "~D-~D" part-number section-number)))))

(defun =internal-cite-2 ()
  (=let* ((paragraph
           (=or
            (=string "paragraph")
            (=string "Paragraph")))
          (space-1 (=zero-or-more (=whitespace)))
          (markers (=zero-or-more (=marker)))
          (space-2 (=zero-or-more (=whitespace)))
          (of-this-section
           (=string "of this section")))
    (=result (format nil "~A~A~{~A~}~A~A"
                     paragraph (list->string space-1)
                     markers (list->string space-2) of-this-section))))

(defun =internal-cite-2->label (part-number section-number)
  (=let* ((_
           (=or
            (=string "paragraph")
            (=string "Paragraph")))
          (_ (=zero-or-more (=whitespace)))
          (markers (=zero-or-more (=marker)))
          (_ (=zero-or-more (=whitespace)))
          (_
           (=string "of this section")))
    (=result (format nil "~D-~D-~{~A~^-~}" part-number section-number
                     (mapcar #'strip-marker markers)))))

(defun =internal-cite-3 ()
  (=let* ((markers (=one-or-more (=marker))))
    (=result (format nil "~{~A~}" markers))))

(defun =full-citation ()
  (=let* ((part-number (=digit-sequence))
          (period (=character #\.))
          (section-number (=digit-sequence))
          (markers (=zero-or-more (=marker))))
    (=result (format nil "~D~C~D~{~A~}" 
                     part-number period section-number markers))))

(defun =full-citation->label ()
  (=let* ((part-number (=digit-sequence))
          (_ (=character #\.))
          (section-number (=digit-sequence))
          (markers (=zero-or-more (=marker))))
    (=result 
     (if markers
         (format nil "~D-~D-~{~A~^-~}" part-number section-number 
                 (mapcar #'strip-marker markers))
         (format nil "~D-~D" part-number section-number)))))

(defun =paragraph-only-citation ()
  (=let* ((markers (=one-or-more (=marker))))
    (=result (format nil "~{~A~}" markers))))

(defun =paragraph-only-citation->label (part-number section-number)
  (=let* ((markers (=zero-or-more (=marker))))
    (=result (format nil "~D-~D-~{~A~^-~}" part-number section-number
                     (mapcar #'strip-marker markers)))))

(defun =internal-citation ()
  (=or
   (=full-citation)
   (=paragraph-only-citation)))

(defun =internal-citation->label (part-number section-number)
  (=or
   (=full-citation->label)
   (=paragraph-only-citation->label part-number section-number)))

(defparameter *test-cite-1* "§ 1026.46(b)(5) foo bar baz")
(defparameter *test-cite-2* "paragraph (b)(1)(i) of this section")
(defparameter *test-cite-3* "§ 1026.46")
(defparameter *test-cite-4* "Creditors offering home-equity plans subject to the requirements of § 1026.40 are not subject to the requirements of paragraph (b)(1)(iv)(A) of this section.")
