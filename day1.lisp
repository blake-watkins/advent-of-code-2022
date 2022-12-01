(in-package :aoc-2022)

(defun parse-elf ()
  (with-monad
    (assign cals (parse-list (parse-number) (parse-newline)))
    (unit (reduce #'+ cals))))

(defun parse-elves ()
  (parse-list (parse-elf) (with-monad (parse-newline) (parse-newline))))

(defun day1 (input)
  (let ((parsed (run-parser (parse-elves) input)))
    
    (iter
      (repeat 3)
      (for elf in (sort parsed #'>))
      (summing elf))
))
