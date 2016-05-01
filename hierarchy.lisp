(in-package :regulations-parser)

(defparameter *all-markers* (list *lowercase-letters* *numerics* *small-romans* *uppercase-letters* *numerics* *big-romans*))

(declaim (ftype (function ((string) (string)) (boolean)) is-successor)
         (ftype (function ((string) (string)) (boolean)) is-descendant))

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defun is-successor (first-marker second-marker)
  "True iff the second marker follows the first marker."
  (loop
     for marker-type in *all-markers*
     do
       (let ((pos-1 (position first-marker marker-type :test #'string=))
             (pos-2 (position second-marker marker-type :test #'string=)))
         (when (and (not (null pos-1))
                    (not (null pos-2))
                    (= (+ 1 pos-1) pos-2))
           (return t)))
     finally (return nil)))

(defun is-descendant (first-marker second-marker)
  "True iff the second marker is a descendant of the first marker."
  (loop
     with root-markers = (first *all-markers*)
     for marker-type in *all-markers*
     if (and (find first-marker root-markers :test #'string=)
             (string= second-marker (first marker-type)))
     do (return t)
     else
     do (setf root-markers marker-type)))

(defstruct marker
  (symbol "" :type string) 
  (depth 0 :type integer)
  (desc-allowed t))

(defun marker-equality (m1 m2)
  (and (string= (marker-symbol m1) (marker-symbol m2))
       (= (marker-depth m1) (marker-depth m2))
       (eql (marker-desc-allowed m1) (marker-desc-allowed m2))))

(defun compute-marker-hierarchy (markers)
  (declare (type list markers)
           (optimize (safety 0) (debug 0) (speed 3)))
  (let* ((depth 1)
         (last-marker-at-depth (make-hash-table))
         (first-marker (make-marker :symbol (first markers)
                                    :depth depth))
         (marker-depths (list first-marker))
         (prev-marker (marker-symbol first-marker))
         (prev-depth depth))
    (declare (type integer depth)
             (type hash-table last-marker-at-depth)
             (type list marker-depths)
             (type string prev-marker)
             (type integer prev-depth))
    (setf (gethash depth last-marker-at-depth) (marker-symbol first-marker))
    (loop
       for current-marker in (the list (rest markers))
       with current-depth = (the integer -1)
       with inserted-marker = nil
       with all-depths = nil
       do
         (setf all-depths (loop for key being the hash-keys of last-marker-at-depth collect key))
         (setf current-depth depth)
         (setf inserted-marker nil)
         (cond ((is-successor prev-marker current-marker)
                (loop
                   for md in marker-depths
                   if (>= (marker-depth md) prev-depth)
                   do (setf (marker-desc-allowed md) nil))
                (let ((new-marker (make-marker :symbol current-marker
                                               :depth prev-depth)))
                  (push new-marker marker-depths)
                  (setf (gethash prev-depth last-marker-at-depth) current-marker)
                  (setf inserted-marker t)))
               ((is-descendant prev-marker current-marker)
                (when (> (+ 1 prev-depth) depth)
                  (incf depth))
                (let ((new-marker (make-marker :symbol current-marker
                                               :depth (+ 1 prev-depth))))
                  (push new-marker marker-depths)
                  (setf (gethash (+ 1 prev-depth) last-marker-at-depth) current-marker)
                  (setf inserted-marker t)
                  (incf prev-depth))))

         (loop
            while (and (find current-depth all-depths)
                       (not inserted-marker))
            do
              (let ((last-marker-at-this-depth (gethash current-depth last-marker-at-depth)))
                (declare (type string last-marker-at-this-depth))
                (cond ((and (is-successor last-marker-at-this-depth current-marker)
                            (find (make-marker :symbol last-marker-at-this-depth :depth current-depth) 
                                  marker-depths :test #'marker-equality))
                       (loop
                          for md in marker-depths
                          if (>= (marker-depth md) current-depth)
                          do (setf (marker-desc-allowed md) nil))
                       (let ((new-marker (make-marker :symbol current-marker :depth current-depth)))
                         (push new-marker marker-depths)
                         (setf (gethash current-depth last-marker-at-depth) current-marker)
                         (setf inserted-marker t)
                         (setf prev-depth current-depth)))
                      ((and (is-descendant last-marker-at-this-depth current-marker)
                            (not (is-descendant prev-marker current-marker))
                            (find (make-marker :symbol last-marker-at-this-depth :depth current-depth)
                                  marker-depths :test #'marker-equality))
                       (when (> (+ 1 current-depth) depth)
                         (incf depth))
                       (let ((new-marker (make-marker :symbol current-marker :depth (+ 1 current-depth))))
                         (push new-marker marker-depths)
                         (setf (gethash (+ 1 current-depth) last-marker-at-depth) current-marker)
                         (setf inserted-marker t)
                         (setf prev-depth (- current-depth 1))))
                      (t
                       (decf current-depth)))))

         (when (and (not inserted-marker)
                    (= current-depth 0))
           (setf depth 1)
           (push (make-marker :symbol current-marker :depth depth) marker-depths)
           (setf (gethash depth last-marker-at-depth) current-marker)
           (setf prev-depth depth))

         (setf prev-marker current-marker)
       finally (return (reverse marker-depths)))))


