(in-package :regulations-parser)

(defstruct part
  (part-number "" :type string)
  (subparts '() :type list))

(defun build-part (part-elem part-number)
  (let* ((subpart-elements (get-elements-by-tag-name part-elem "SUBPART"))
         (section-elements
          (unless subpart-elements
            (get-elements-by-tag-name part-elem "SECTION")))
         (subparts
          (loop
             for subpart-elem in subpart-elements
             collect
               (build-subpart subpart-elem part-number)))
         (sections
          (reverse
           (loop
	      for section-elem in section-elements
	      collect
		(build-section section-elem part-number))))
         (dummy-subpart
          (unless subparts
            (list
             (make-subpart :label (format nil "~D-Subpart" part-number)
                           :sections sections)))))
    ;; first scan the entire tree for definitions
    (loop
       for section in sections
       do
         (find-and-interpolate-definitions (section-paragraphs section)))
    ;; now that the defined terms are in the hash, scan and
    ;; interpolate references to those terms
    (loop
       for section in sections
       do
         (scan-and-interpolate-termrefs-and-cites (section-paragraphs section)
                                                  part-number (section-section-number section)))
    (make-part :part-number part-number
               :subparts dummy-subpart)))
    

