(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-number-list)))

(defun neighbours (point)
  (mapcar (lambda (d) (point+ point d)) '((1 0 0) (-1 0 0)
                                          (0 1 0) (0 -1 0)
                                          (0 0 1) (0 0 -1))))

(defun map-dimensions (map)
  (iter
    (with max = nil)
    (with min = nil)
    (for (point nil) in-hashtable map)
    (setf max (point-max max point))
    (setf min (point-min min point))
    (finally (return (list min max)))))

(defun get-exterior (map)
  (destructuring-bind (min max) (map-dimensions map)
    (setf min (point- min '(1 1 1)))
    (setf max (point+ max '(1 1 1)))
    (labels ((valid-exterior-point (point)
               (and (every (lambda (min val max) (<= min val max))
                           min point max)
                    (not (gethash point map)))))
      (iter
        (with exterior = (make-hash-table :test 'equal))
        (for (point) in-bfs-from min             
             neighbours (lambda (point)
                          (remove-if-not #'valid-exterior-point
                                         (neighbours point)))
             test 'equal
             single t)
        (setf (gethash point exterior) t)
        (finally (return exterior))))))

(defun day18 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (map (iter
                (with map = (make-hash-table :test 'equal))
                (for point in parsed)
                (setf (gethash point map) t)
                (finally (return map))))
         (exterior (get-exterior map)))
    (iter
      (for point in parsed)
      (for neighbours = (neighbours point))
      (summing
       (iter
         (for n in neighbours)
         (counting (and (not (gethash n map))
                        (or (= part 1) (gethash n exterior)))))))))


