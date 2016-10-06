(in-package :regulations-parser)

(defun build-toc (notice-root result-root)
  (let* ((regtext (select-by (where :tag "REGTEXT") notice-root))
         (part-number (get-attribute regtext "PART"))
         (part (select-by (where :tag "PART") regtext))
         (contents (select-by (where :tag "CONTENTS") part))
         (section-numbers (nreverse (select-by (where :tag "SECTNO") contents)))
         (section-subjects (nreverse (select-by (where :tag "SUBJECT") contents)))
         (appendices (nreverse (select-by
				(where :tag "FP" :attribute '("SOURCE" "FP-2")) notice-root)))
         (table-of-contents (make-element result-root "tableOfContents")))
    (loop
       for sec-num in section-numbers
       for sec-sub in section-subjects
       do
         (let* ((target (regex-replace "\\." (text sec-num) "-"))
                (sec-number (second (split "-" target)))
                (sec-entry (make-element table-of-contents "tocSecEntry"))
                (sec-num-elem (make-element sec-entry "sectionNum"))
                (sec-subject (make-element sec-entry "sectionSubject")))
           (set-attribute sec-entry "target" target)
           (make-text-node sec-num-elem sec-number)
           (make-text-node sec-subject (text sec-sub))))
    (loop
       for appendix-elem in appendices
       do
         (let* ((appendix-subject (text appendix-elem))
                (appendix-letter
                 (cdr (run (=appendix) appendix-subject))))
           (when (not (null appendix-letter))
             (let* ((appendix-entry (make-element table-of-contents "tocAppEntry"))
                    (appendix-letter-elem (make-element appendix-entry "appendixLetter"))
                    (appendix-subject-elem (make-element appendix-entry "appendixSubject")))
               ;; (format t "~A ~A~%" appendix-letter appendix-subject)
               (set-attribute appendix-entry "target"
			      (format nil "~D-~A" part-number appendix-letter))
               (make-text-node appendix-letter-elem appendix-letter)
               (make-text-node appendix-subject-elem appendix-subject)))))
    table-of-contents))
