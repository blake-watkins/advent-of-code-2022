(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (one-or-more (parse-alphanumeric))))

(defun common (rucksack)
  (let* ((half (floor (length rucksack) 2))
         (front (subseq rucksack 0 half))
         (back (subseq rucksack half (length rucksack))))
    (iter
      (for c in front)
      (finding c such-that (member c back)))))

(defun priority (c)
  (if (lower-case-p c)
      (+ (- (char-code c) (char-code #\a)) 1)
      (+ (- (char-code c) (char-code #\A)) 27)))

(defun day3 (input)
  (reduce #'+
          (mapcar (lambda (rucksack) (priority (common rucksack)))
                  (run-parser (parse-file) input))))

(defun parse-file-2 ()
  (one-or-more (n-of 3 (with-monad
                         (assign ret (one-or-more (parse-alphanumeric)))
                         (parse-newline)
                         (unit ret)))))

(defun common-2 (rucksacks)
  (iter
    (for c in (first rucksacks))
    (finding c such-that (every (lambda (rucksack) (member c rucksack))
                                rucksacks))))

(defun day3-2 (input)
  (reduce #'+
          (mapcar (lambda (rucksacks) (priority (common-2 rucksacks)))
                  (run-parser (parse-file-2) input))))
