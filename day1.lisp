(in-package :aoc-2022)

(defun parse-elf ()
  (with-monad
    (assign calories (parse-list (parse-number) (parse-newline)))
    (unit (reduce #'+ calories))))

(defun parse-elves ()
  (parse-list (parse-elf) (n-of 2 (parse-newline))))

(defun day1 (input &key (part 1))
  (let ((parsed (sort (run-parser (parse-elves) input) #'>)))
    (if (= part 1)
	(apply #'max parsed)
	(iter
	  (repeat 3)
	  (for elf in parsed)
	  (summing elf)))))
