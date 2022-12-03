(in-package :aoc-2022)

(defun parse-rucksack ()
  (parse-line (one-or-more (parse-alphanumeric))))

(defun parse-rucksacks ()
  (one-or-more (parse-rucksack)))

(defun parse-groups ()
  (one-or-more (n-of 3 (parse-rucksack))))

(defun split-rucksack (rucksack)
  (let* ((half (floor (length rucksack) 2))
         (front (subseq rucksack 0 half))
         (back (subseq rucksack half (length rucksack))))
    (list front back)))

(defun common (rucksacks)
  (iter
    (for c in (first rucksacks))
    (finding c such-that
             (every (lambda (rucksack) (member c rucksack)) rucksacks))))

(defun priority (c)
  (if (lower-case-p c)
      (+ (- (char-code c) (char-code #\a)) 1)
      (+ (- (char-code c) (char-code #\A)) 27)))

(defun day3 (input &key (part 1))
  (reduce #'+
          (if (= part 1)
              (mapcar (lambda (rucksack)
                        (priority (common (split-rucksack rucksack))))
                      (run-parser (parse-rucksacks) input))
              (mapcar (lambda (rucksacks) (priority (common rucksacks)))
                      (run-parser (parse-groups) input)))))


