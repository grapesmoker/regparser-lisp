(in-package :regulations-parser)

(defstruct regulation-meta
  (effective-date "" :type string)
  (title "" :type string)
  (url "" :type string)
  (pub-date "" :type string)
  (cfr-title "" :type string)
  (cfr-part "" :type string)
  (document-number "" :type string))


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
