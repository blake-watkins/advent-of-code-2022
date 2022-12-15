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
     (cond
       ((and (null a) (null b)) :equal)
       ((null a) :less)
       ((null b) :more)
       (t (let ((cmp (compare (car a) (car b))))
	    (if (eq cmp :equal)
		(compare (cdr a) (cdr b))
		cmp)))))
    ((numberp a) (compare (list a) b))
    ((numberp b) (compare a (list b)))))

(defparameter *divider-packets* '(((2)) ((6))))

(defun packet< (a b) (eq :less (compare a b)))

(defun index-of (packet packets)
  (1+ (position packet packets :test 'equal)))

(defun all-packets (parsed)
  (append *divider-packets* (iter (for (a b) in parsed) (collect a) (collect b))))

(defun day13 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (if (= part 1)
	(iter
	  (for index from 1)
	  (for (a b) in parsed)
	  (when (packet< a b) (sum index)))
	(let ((sorted (sort (all-packets parsed) #'packet<)))
	  (reduce #'* (mapcar (lambda (packet) (index-of packet sorted))
			      *divider-packets*))))))
