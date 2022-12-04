(in-package :aoc-2022)

(defun parse-assignment ()
  (parse-list (parse-number) (parse-character #\-)))

(defun parse-file ()
  (parse-lines (parse-list (parse-assignment))))

(defun day4 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for ((s1 e1) (s2 e2)) in parsed)
      (counting
       (if (= part 1)
           (or (and (>= s1 s2) (<= e1 e2))
               (and (>= s2 s1) (<= e2 e1)))
           (or (<= s2 s1 e2)
               (<= s2 e1 e2)
               (<= s1 s2 e1)
               (<= s1 e2 e1)))))))
