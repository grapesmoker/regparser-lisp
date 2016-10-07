(in-package :regulations-parser)

(defstruct regulation-meta
  (effective-date "" :type string)
  (title "" :type string)
  (url "" :type string)
  (pub-date "" :type string)
  (cfr-title "" :type string)
  (cfr-part "" :type string)
  (document-number "" :type string))

(defstruct fdsys
  (cfr-title-num "" :type string)
  (cfr-title-text "" :type string)
  (volume "" :type string)
  (date "" :type string)
  (original-date "" :type string)
  (title "" :type string))

(defstruct cfr
  (title "" :type string)
  (section "" :type string))

(defstruct preamble
  (agency "" :type string)
  (reg-letter "" :type string)
  (cfr (make-cfr) :type cfr)
  (document-number "" :type string)
  (effective-date "" :type string)
  (federal-register-url "" :type string))

(defstruct meta
  (fdsys (make-fdsys) :type fdsys)
  (preamble (make-preamble) :type preamble))

(defun fdsys->xml (fdsys &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<fdsys>~%" (indent (* depth indent))))
	(end-tag (format nil "~A</fdsys>~%" (indent (* depth indent))))
	(cfr-title-num
	 (format nil "~A<cfrTitleNum>~A</cfrTitleNum>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-cfr-title-num fdsys)))
	(cfr-title-text
	 (format nil "~A<cfrTitleText>~A</cfrTitleText>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-cfr-title-text fdsys)))
	(volume
	 (format nil "~A<volume>~A</volume>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-volume fdsys)))
	(date
	 (format nil "~A<date>~A</date>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-date fdsys)))
	(original-date
	 (format nil "~A<originalDate>~A</originalDate>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-original-date fdsys)))
	(title
	 (format nil "~A<title>~A</title>~%"
		 (indent (* (+ depth 1) indent))
		 (fdsys-title fdsys))))
    (format nil "~A~A~A~A~A~A~A~A"
	    start-tag
	    cfr-title-num
	    cfr-title-text
	    volume
	    date
	    original-date
	    title
	    end-tag)))

(defun cfr->xml (cfr &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<cfr>~%" (indent (* depth indent))))
	(end-tag (format nil "~A</cfr>~%" (indent (* depth indent))))
	(title (format nil "~A<title>~A</title>~%"
		       (indent (* (+ 1 depth) indent)) (cfr-title cfr)))
	(section (format nil "~A<section>~A</section>~%"
			 (indent (* (+ 1 depth) indent)) (cfr-section cfr))))
    (format nil "~A~A~A~A" start-tag title section end-tag)))

(defun preamble->xml (preamble &optional (depth 0) (indent 4))
  (let ((start-tag (format nil "~A<preamble>~%" (indent (* depth indent))))
	(end-tag (format nil "~A</preamble>~%" (indent (* depth indent))))
    	(agency
	 (format nil "~A<agency>~A</agency>~%"
		 (indent (* (+ depth 1) indent))
		 (preamble-agency preamble)))
    	(reg-letter
	 (format nil "~A<regLetter>~A</regLetter>~%"
		 (indent (* (+ depth 1) indent))
		 (preamble-reg-letter preamble)))
	(cfr (cfr->xml (preamble-cfr preamble) (+ 1 depth) indent))
    	(document-number
	 (format nil "~A<documentNumber>~A</documentNumber>~%"
		 (indent (* (+ depth 1) indent))
		 (preamble-document-number preamble)))
    	(effective-date
	 (format nil "~A<effectiveDate>~A</effectiveDate>~%"
		 (indent (* (+ depth 1) indent))
		 (preamble-effective-date preamble)))
    	(federal-register-url
	 (format nil "~A<federalRegisterURL>~A</federalRegisterURL>~%"
		 (indent (* (+ depth 1) indent))
		 (preamble-federal-register-url preamble))))
    (format nil "~A~A~A~A~A~A~A~A"
	    start-tag agency reg-letter cfr document-number effective-date
	    federal-register-url end-tag)))

(defun meta->xml (meta &optional (depth 0) (indent 4))
  (format nil "~A~A"
	  (fdsys->xml (meta-fdsys meta) depth indent)
	  (preamble->xml (meta-preamble meta) depth indent)))
	  
	
(defun build-meta (document-number)
  (let* ((reg-meta-raw
	  (http-request
	   (format nil "https://www.federalregister.gov/api/v1/articles/~A" document-number)))
         (reg-meta-json (decode-json-from-string (flexi-streams:octets-to-string reg-meta-raw)))
         (title (cdr (assoc :title reg-meta-json)))
         (url (cdr (assoc :html--url reg-meta-json)))
         (doc-number (cdr (assoc :document--number reg-meta-json)))
         (effective-date (cdr (assoc :effective--on reg-meta-json)))
         (pub-date (cdr (assoc :publication--date reg-meta-json)))
	 (volume (write-to-string (cdr (assoc :volume reg-meta-json))))
         (cfr-title (write-to-string (cdaadr (assoc :cfr--references reg-meta-json))))
         (cfr-part (write-to-string (cdr (cadadr (assoc :cfr--references reg-meta-json)))))
	 (cfr-title-text (cadr (assoc (read-from-string cfr-title) *cfr-titles*)))
         (agency (cdr (assoc :name (cadr (assoc :agencies reg-meta-json)))))
	 (fdsys
	  (make-fdsys :cfr-title-num cfr-title
		      :cfr-title-text cfr-title-text
		      :volume volume
		      :date pub-date
		      :original-date pub-date
		      :title title))
	 (cfr
	  (make-cfr :title cfr-title
		    :section cfr-part))
	 (preamble
	  (make-preamble :agency agency
			 :cfr cfr
			 :document-number doc-number
			 :effective-date effective-date
			 :federal-register-url url))
	 (meta
	  (make-meta :fdsys fdsys
		     :preamble preamble)))
    meta))
