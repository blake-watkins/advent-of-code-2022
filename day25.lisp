(in-package :aoc-2022)

(defun parse-snafu ()
  (one-or-more
   (with-monad
     (assign char (parse-character "=-012"))
     (unit (- (position char "=-012" :test #'char=) 2)))))

(defun snafu-to-base10 (snafu &key (acc 0))
  (if (null snafu)
      acc
      (snafu-to-base10 (cdr snafu) :acc (+ (* acc 5) (car snafu)))))

(defun base10-to-snafu (num)
  (labels ((to-digits (num)
             (if (<= num 2)
                 (list num)
                 (multiple-value-bind (q r) (floor (+ num 2) 5)
                   (cons (- r 2) (to-digits q))))))
    (reverse (to-digits num))))

(defun format-snafu (snafu)
  (format nil "~{~a~}" (mapcar (lambda (d) (elt "=-012" (+ d 2))) snafu)))

(defun day25 (input)
  (let ((parsed (run-parser (parse-lines (parse-snafu)) input)))
    (format-snafu
     (base10-to-snafu
      (reduce #'+ (mapcar #'snafu-to-base10 parsed))))))
