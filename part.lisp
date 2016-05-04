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
               (build-section section-elem))))
         (dummy-subpart
          (unless subparts
            (list
             (make-subpart :label (format nil "~D-Subpart" part-number)
                           :sections sections)))))
    (make-part :part-number part-number
               :subparts dummy-subpart)))
    

