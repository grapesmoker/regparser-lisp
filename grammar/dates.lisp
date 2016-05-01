(in-package :regparser)

(defparameter *months*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun =month ()
  (=one-of-words *months*))

(defun =full-date ()
  (=let* ((month (=month))
          (day (=skip-whitespace
                           (=one-or-more
                            (=digit))))
          (_ (=skip-whitespace
              (=character #\,)))
          (year (=skip-whitespace
                 (=exactly 4 (=digit)))))
    (=result (list (cons 'MONTH month)
                   (cons 'DAY (stringify day))
                   (cons 'YEAR (stringify year))))))

(defun =effective-date ()
  (=zero-or-more
   (=or
    (=let* ((eff-on (=skip-whitespace (=or (=string "effective on")
                                           (=string "Effective on"))))
            (eff-date (=skip-whitespace (=full-date))))
      (if (and eff-on eff-date)
          (=result (cons 'EFFECTIVE-ON eff-date))
          (=fail)))
    (=skip-whitespace (=word)))))

(defun =eff-date ()
  (=zero-or-more
   (=or
    (=let* ((eff-on (=or (=string "effective on")
                         (=string "Effective on")))
            (eff-date (=skip-whitespace
                       (=full-date))))
      (if (and eff-on)
          (=result (cons 'EFF-DATE 'foo))))
    (=item))))
                    
