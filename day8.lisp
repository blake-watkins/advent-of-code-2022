(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (one-or-more (parse-digit))))

(defun get-tree (r c trees)
  (elt (elt trees r) c))

(defun visible? (r c max-r max-c trees)
  (let ((tree (get-tree r c trees)))
    (or (iter
	  (for i below r)
	  (never (>= (get-tree i c trees) tree)))
	(iter
	  (for i from (1+ r) below max-r)
	  (never (>= (get-tree i c trees) tree)))
	(iter
	  (for i below c)
	  (never (>= (get-tree r i trees) tree)))
	(iter
	  (for i from (1+ c) below max-c)
	  (never (>= (get-tree r i trees) tree))))))

(defun day8 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (max-r (length parsed))
	 (max-c (length (first parsed))))
    (iter outer
      (for r below max-r)
      (iter
	(for c below max-c)
	(when (visible? r c max-r max-c parsed)
	  (in outer (summing 1)))))))
