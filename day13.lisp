(in-package :aoc-2022)

(defun parse-packet ()
  (parse-bracketed
   (either (parse-list (either (parse-number) (parse-packet)) ",")
	   (unit '()))
   "[]"))

(defun parse-file ()
  (parse-lines (n-of 2 (parse-line (parse-packet)))))

(defun compare (a b)
  (cond
    ((and (numberp a) (numberp b))
     (cond ((< a b) :less) ((= a b) :equal) (t :more)))
    ((and (listp a) (listp b))
     (let ((ret (iter
		  (for elem-a in a)
		  (for elem-b in b)
		  (for cmp = (compare elem-a elem-b))
		  (finding cmp such-that (not (eq :equal cmp))))))
       (if (or (null ret) (eq :equal ret))
	   (cond
	     ((< (length a) (length b)) :less)
	     ((= (length a) (length b)) :equal)
	     (t :more))
	   ret)))
    ((numberp a) (compare (list a) b))
    (t (compare a (list b)))))

(defun day13 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (comparisons (mapcar (lambda (pair) (apply #'compare pair)) parsed)))
    (iter
      (for i from 1)
      (for c in comparisons)
      (when (eq :less c)
	(sum i)))))

(defun day13-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (packets (all-packets parsed))
	 (sorted (sort packets (lambda (a b) (eq :less (compare a b))))))
    (* (1+ (position '((2)) sorted :test 'equal))
       (1+ (position '((6)) sorted :test 'equal))
       )))

(defun all-packets (parsed)
  (concatenate 'list
	       '(((2)) ((6)))
	       (iter
		 (for pair in parsed)
		 (collect (first pair))
		 (collect (second pair)))))
