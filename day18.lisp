(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-number-list)))

(defun neighbours (point)
  (mapcar (lambda (d) (point+ point d)) '((1 0 0) (-1 0 0)
                                          (0 1 0) (0 -1 0)
                                          (0 0 1) (0 0 -1))))
(defun day18 (input)
  (let ((parsed (run-parser (parse-file) input))
        (map (make-hash-table :test 'equal)))
    (iter
      (for point in parsed)
      (setf (gethash point map) t))
    (iter
      (for point in parsed)
      (for neighbours = (neighbours point))
      (summing
       (iter
         (for n in neighbours)
         (counting (not (gethash n map))))))))
