(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (one-or-more (parse-digit))))

(defparameter *directions* '((1 0) (-1 0) (0 1) (0 -1)))

(defun get-tree (pos trees)
  (destructuring-bind (r c) pos
    (if (<= 0 r (1- (length trees)))
	(let ((row (elt trees r)))
	  (if (<= 0 c (1- (length row)))
	      (elt row c)
	      nil))
	nil)))

(defun visible-trees-in-direction (pos dir trees &key (prev-height -1))
  (let* ((next-pos (map 'list #'+ pos dir))
	 (next (get-tree next-pos trees)))
    (if next
	(if (>= next prev-height)
	    (1+ (visible-trees-in-direction next-pos
					    dir
					    trees
					    :prev-height next))
	    (visible-trees-in-direction next-pos
					dir
					trees
					:prev-height prev-height))
	0)))

(defun visible? (pos trees)
  (some (lambda (dir)
	  (= 0 (visible-trees-in-direction pos
					   dir
					   trees
					   :prev-height (get-tree pos trees))))
	*directions*))

(defun view-distance-in-direction (height pos dir trees)
  (let* ((next-pos (map 'list #'+ pos dir))
	 (next (get-tree next-pos trees)))
    (if next
	(if (>= next height)
	    1
	    (1+ (view-distance-in-direction height
					    next-pos
					    dir
					    trees)))
	0)))

(defun scenic-score (pos trees)
  (apply #'* (mapcar (lambda (dir)
		       (view-distance-in-direction (get-tree pos trees)
						   pos
						   dir
						   trees))
		     *directions*)))

(defun day8 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (max-r (length parsed))
	 (max-c (length (first parsed))))
    (iter outer
      (for r below max-r)
      (iter
	(for c below max-c)
	(when (visible? (list r c) parsed)
	  (in outer (summing 1)))))))

(defun day8-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (max-r (length parsed))
	 (max-c (length (first parsed))))
    (iter outer
      (for r below max-r)
      (iter
	(for c below max-c)
	(in outer (maximizing (scenic-score (list r c) parsed)))))))

