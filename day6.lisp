(in-package :aoc-2022)

(defun parse-file ()
  (one-or-more (parse-alphanumeric)))

(defun day6 (input &key (part 1))
  (let ((search-length (if (= part 1) 4 14))
        (parsed (run-parser (parse-file) input)))
    (iter
      (for i from 0)
      (until (and (> i (1- search-length))
                  (= search-length
                     (length (remove-duplicates
                              (subseq parsed (- i search-length) i))))))
      (finally (return i)))))
