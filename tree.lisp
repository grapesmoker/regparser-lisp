(in-package :regulations-parser)

;; Find nodes in the tree based on a predicate
(defun select-by (predicate root)
  (let ((accum '()))
    (if (and
	 (element-p root)
         (funcall predicate root))
         (push root accum))
    (labels ((collector (node)
               (loop 
                  for child across (if (or (root-p node) (element-p node))
                                       (children node)
                                       #())
                  do
                    (if (and
                         (element-p child)
                         (funcall predicate child))
                        (push child accum))
                    (collector child))))
      (collector root))
    (if (> (length accum) 1)
        accum
        (first accum))))
           
;; Build a selector predicate to avoid lambda verbosity
(defun where (&key tag attribute)
  #'(lambda (node)
      (and
       (if tag 
           (string= (tag-name node) tag)
           t)
       (if attribute
           (let ((attr-name (first attribute))
                 (attr-value (second attribute)))
             (if (and 
                  (has-attribute node attr-name)
                  (string= (attribute node attr-name)
                           attr-value))
                 t))
           t))))
                   
(defun xml-tree->string (root)
  (serialize root))

