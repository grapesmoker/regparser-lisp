;;;; regulations-parser.lisp

(in-package :regulations-parser)

(defun load-xml-tree (filename)
  (parse filename))

(defun nth-child (n node)
  (aref (children node) n))

(defstruct regulation-meta
  (effective-date "" :type string)
  (title "" :type string)
  (url "" :type string)
  (pub-date "" :type string)
  (cfr-title "" :type string)
  (cfr-part "" :type string)
  (document-number "" :type string))

(defun parser-driver (notice-file document-number)
  (let* ((notice-xml (load-xml-tree notice-file))
         (root-result (make-root))
         (reg-meta (build-meta root-result document-number))
         (toc-xml (build-toc notice-xml root-result))
         (regtext-xml (build-regtext notice-xml root-result reg-meta))
         (appendix-xml (build-appendices notice-xml root-result))
         (interp-xml (build-interps notice-xml root-result)))
    root-result))

(defun build-meta (result-root document-number)
  (let* ((reg-meta-raw (http-request (format nil "https://www.federalregister.gov/api/v1/articles/~A" document-number)))
         (reg-meta-json (decode-json-from-string (flexi-streams:octets-to-string reg-meta-raw)))
         (title (cdr (assoc :title reg-meta-json)))
         (url (cdr (assoc :html--url reg-meta-json)))
         (doc-number (cdr (assoc :document--number reg-meta-json)))
         (effective-date (cdr (assoc :effective--on reg-meta-json)))
         (pub-date (cdr (assoc :publication--date reg-meta-json)))
         (cfr-title (write-to-string (cdaadr (assoc :cfr--references reg-meta-json))))
         (cfr-part (write-to-string (cdr (cadadr (assoc :cfr--references reg-meta-json)))))
         (agency (cdr (assoc :name (cadr (assoc :agencies reg-meta-json)))))
         (reg-meta (make-regulation-meta
                    :effective-date effective-date
                    :url url
                    :title title
                    :cfr-title cfr-title
                    :cfr-part cfr-part
                    :pub-date pub-date
                    :document-number doc-number)))
    (let* ((preamble-element (make-element result-root "preamble"))
           (agency-elem (make-element preamble-element "agency"))
           (cfr (make-element preamble-element "cfr"))
           (cfr-title-elem (make-element cfr "title"))
           (cfr-section-elem (make-element cfr "section"))
           (doc-number-elem (make-element preamble-element "documentNumber"))
           (effective-date-elem (make-element preamble-element "effectiveDate"))
           (url-elem (make-element preamble-element "federalRegisterURL")))
      (make-text-node effective-date-elem effective-date)
      (make-text-node agency-elem agency)
      (make-text-node cfr-title-elem cfr-title)
      (make-text-node cfr-section-elem cfr-part)
      (make-text-node url-elem (regulation-meta-url reg-meta))
      (make-text-node doc-number-elem doc-number)
      (serialize preamble-element)
      preamble-element)))
         


(defun build-toc (notice-root result-root)
  (let* ((regtext (select-by (where :tag "REGTEXT") notice-root))
         (part (select-by (where :tag "PART") regtext))
         (contents (select-by (where :tag "CONTENTS") part))
         (section-numbers (nreverse (select-by (where :tag "SECTNO") contents)))
         (section-subjects (nreverse (select-by (where :tag "SUBJECT") contents)))
         (appendices (nreverse (select-by (where :tag "FP" :attribute '("SOURCE" "FP-2")) notice-root)))
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
               (set-attribute appendix-entry "target" appendix-letter)
               (make-text-node appendix-letter-elem appendix-letter)
               (make-text-node appendix-subject-elem appendix-subject)))))
    (serialize table-of-contents)
    table-of-contents))
         


(defun build-regtext (notice-xml result-root reg-meta)
  ())

(defun build-appendices (notice-xml result-root)
  ())

(defun build-interps (notice-xml result-root)
  ())
