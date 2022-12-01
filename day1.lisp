(in-package :aoc-2022)

(defun parse-elf ()
  (parse-list (parse-number) (parse-newline)))

(defun parse-elves ()
  (parse-list (parse-elf) (n-of 2 (parse-newline))))

(defun day1 (input &key (part 1))
  (let* ((parsed (run-parser (parse-elves) input))
	 (calories (mapcar (lambda (foods) (reduce #'+ foods)) parsed))
	 (sorted (sort calories #'>)))
    (if (= part 1)
	(first sorted)
	(reduce #'+ (subseq sorted 0 3)))))
