(in-package :regulations-parser)

(defstruct subpart
  (label "" :type string)
  (sections '() :type list))

(defun build-subpart (subpart-elem part-number)
  ())
