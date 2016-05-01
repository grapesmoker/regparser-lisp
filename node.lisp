(defclass reg-node ()
  ((title :initarg :title 
          :initform nil
          :accessor reg-node-title)
   (text :initarg :text
         :initform nil
         :accessor reg-node-text)
   (label :initarg :label
          :initform nil
          :accessor reg-node-label)
   (marker :initarg :marker
           :initform nil
           :accessor reg-node-marker)))
