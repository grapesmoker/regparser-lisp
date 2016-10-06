(in-package :regulations-parser)

(defparameter *cfr-titles*
  '((1 "GENERAL PROVISIONS")
    (2 "THE CONGRESS")
    (3 "THE PRESIDENT")
    (4 "FLAG AND SEAL, SEAT OF GOVERNMENT, AND THE STATES")
    (5 "GOVERNMENT ORGANIZATION AND EMPLOYEES")
    (6 "DOMESTIC SECURITY")
    (7 "AGRICULTURE")
    (8 "ALIENS AND NATIONALITY")
    (9 "ARBITRATION")
    (10 "ARMED FORCES")
    (11 "BANKRUPTCY")
    (12 "BANKS AND BANKING")
    (13 "CENSUS")
    (14 "COAST GUARD")
    (15 "COMMERCE AND TRADE")
    (16 "CONSERVATION")
    (17 "COPYRIGHTS")
    (18 "CRIMES AND CRIMINAL PROCEDURE")
    (19 "CUSTOMS DUTIES")
    (20 "EDUCATION")
    (21 "FOOD AND DRUGS")
    (22 "FOREIGN RELATIONS AND INTERCOURSE")
    (23 "HIGHWAYS")
    (24 "HOSPITALS AND ASYLUMS")
    (25 "INDIANS")
    (26 "INTERNAL REVENUE CODE")
    (27 "INTOXICATING LIQUORS")
    (28 "JUDICIARY AND JUDICIAL PROCEDURE")
    (29 "LABOR")
    (30 "MINERAL LANDS AND MINING")
    (31 "MONEY AND FINANCE")
    (32 "NATIONAL GUARD")
    (33 "NAVIGATION AND NAVIGABLE WATERS")
    (35 "PATENTS")
    (36 "PATRIOTIC AND NATIONAL OBSERVANCES, CEREMONIES, AND ORGANIZATIONS")
    (37 "PAY AND ALLOWANCES OF THE UNIFORMED SERVICES")
    (38 "VETERANS’ BENEFITS")
    (39 "POSTAL SERVICE")
    (40 "PUBLIC BUILDINGS, PROPERTY, AND WORKS")
    (41 "PUBLIC CONTRACTS")
    (42 "THE PUBLIC HEALTH AND WELFARE")
    (43 "PUBLIC LANDS")
    (44 "PUBLIC PRINTING AND DOCUMENTS")
    (45 "RAILROADS")
    (46 "SHIPPING")
    (47 "TELECOMMUNICATIONS")
    (48 "TERRITORIES AND INSULAR POSSESSIONS")
    (49 "TRANSPORTATION")
    (50 "WAR AND NATIONAL DEFENSE")
    (51 "NATIONAL AND COMMERCIAL SPACE PROGRAMS")
    (52 "VOTING AND ELECTIONS")
    (54 "NATIONAL PARK SERVICE AND RELATED PROGRAMS")))

(defparameter *indent* 4)

(defun cfr-title (num)
  (second (assoc num *cfr-titles*)))

(defun flatten (a-list)
  (cond ((null a-list) nil)
        ((atom a-list) (list a-list))
        (t (mapcan #'flatten a-list))))

(defun stringify (a-list)
  (let ((flat-list (flatten a-list)))
    (cond ((every #'stringp flat-list)
           (append flat-list))
          ((every #'characterp flat-list)
           (append (concatenate 'string flat-list)))
          (t
           (append (mapcar #'string flat-list))))))
           ;;(concatenate 'string flat-list)))))

(defun collect-result-by (parser-result tag)
  (remove-if-not #'(lambda (elem)
                     (and (consp elem)
                          (eq (car elem) tag)))
                 parser-result))

(defun indent (num-spaces)
  (format nil 
          (format nil "~~~D<~~>" num-spaces)
          " "))

(defun list->string (a-list)
  (format nil "~{~A~}" a-list))

(defun split-at-locations (text start-end-list)
  (loop
     for start-end-pair in start-end-list
     with current-start = 0
     collect
       (let* ((start (first start-end-pair))
              (end (second start-end-pair))
              (current-end start)
              (piece (subseq text current-start current-end)))
         (setf current-start end)
         piece)
     into result
     finally
       (return (append result (list (subseq text (second start-end-pair)))))))

(defun insert-into-locations (text values start-end-list)
  (unless (= (length values) (length start-end-list))
    (error (format nil "Number of values must be equal to number of start-end pairs!~%Values: ~A~%Indices: ~A"
		   values start-end-list)))
  (let* ((split-text (split-at-locations text start-end-list)))
    (list->string
     (loop
	for val in values
	for piece in split-text
	append
	  (list piece val)
	into result
	finally
	  (return (append result (last split-text)))))))


(defun extract-all-tokens (parser text &key (token-name :token))
  ;; Given a parser that extracts a particular token, this function
  ;; extracts all tokens matched by that parser from the text and
  ;; reports them as triples of (:token :start-position :end-position) 
  (labels ((=next-token ()
             (=let* ((_
                      (=zero-or-more
                       (=not (funcall parser))))
                     (markers
                      (=zero-or-more
                       (funcall parser))))
               (=result markers))))
    (loop
       with end-index = 0
       with start-index = 0
       with tokens = '()
       with current-text = text
       while (< end-index (length text))
       do
         (let* ((next-token-loc (run (=next-token) current-text :result #'identity))
                (next-token (list->string (caar next-token-loc)))
                (next-end (mpc::index-simple-string-position (cdar next-token-loc))))
                ;; (next-start (- next-end (length next-token))))
           (incf end-index next-end)
           ;; (format t "~A ~A ~A ~A ~A~%" next-token next-end next-start end-index (length next-token))
           (setf current-text (subseq text end-index))
           (setf start-index (- end-index (length next-token)))
           (when (not (string= "" next-token))
             (push (list 
                    (cons token-name next-token)
                    (cons :start start-index)
                    (cons :end end-index)) tokens)))
       finally
         (return (reverse tokens)))))

(defun strip-marker (marker)
  (regex-replace #\( 
                 (regex-replace #\)
                                (regex-replace #\. marker "")
                                "")
                 ""))
