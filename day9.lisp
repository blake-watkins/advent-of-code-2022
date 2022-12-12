(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-list (either (parse-number) (parse-keyword)) " ")))

(defun move (pos dir)
  (destructuring-bind (r c) pos
    (ecase dir
      (:U (list (1- r) c))
      (:D (list (1+ r) c))
      (:L (list r (1- c)))
      (:R (list r (1+ c))))))

(defun update-tail (head tail)
  (let ((diff (map 'list #'- head tail)))
    (cond
      ((every (lambda (d) (<= (abs d) 1)) diff) tail)
      (t (map 'list #'+ tail (mapcar #'signum diff))))))

(defun day9 (input &key (part 1))
  (let ((num-knots (if (= part 1) 2 10))
        (moves (run-parser (parse-file) input)))
    (iter
      (with knots = (iter (repeat num-knots) (collect '(0 0))))
      (with tail-positions = (make-hash-table :test 'equal))
      (for (direction amount) in moves)
      (iter
        (repeat amount)
        (setf (elt knots 0) (move (elt knots 0) direction))
        (iter
          (for i from 1 below num-knots)
          (setf (elt knots i) (update-tail (elt knots (1- i)) (elt knots i))))
        (setf (gethash (elt knots (1- num-knots)) tail-positions) t))
      (finally (return (hash-table-count tail-positions))))))
