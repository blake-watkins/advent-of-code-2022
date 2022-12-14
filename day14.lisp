(in-package :aoc-2022)

(defun parse-coordinate ()
  (parse-number-list))

(defun parse-path ()
  (parse-list (parse-coordinate) " -> "))

(defun parse-file ()
  (parse-lines (parse-path)))

(defun occupied (pos max-y map)
  (destructuring-bind (x y) pos
    (declare (ignore x))
    (or (= y (+ 2 max-y)) (gethash pos map))))

(defun move-sand (sand max-y map)
  (destructuring-bind (x y) sand
    (cond
      ((not (occupied (list x (1+ y)) max-y map))
       (list :falling (list x (1+ y))))
      ((not (occupied (list (1- x) (1+ y)) max-y map))
       (list :falling (list (1- x) (1+ y))))
      ((not (occupied (list (1+ x) (1+ y)) max-y map))
       (list :falling (list (1+ x) (1+ y))))
      (t (list :landed (list x y))))))

(defun day14 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (map (build-map parsed))
         (max-y (iter
                  (for ((x y) ignore) in-hashtable map)
                  (maximizing y))))
    (iter
      (for i from 0)
      (until (gethash '(500 0) map))
      (for res = (iter
                   (with pos = '(500 0))
                   (for (res new-pos) = (move-sand pos max-y map))
                   (setf pos new-pos)
                   (when (eq :landed res)
                     (setf (gethash pos map) t))
                   (until (or (eq :landed res) (eq :void res)))
                   (finally (return res))))
      (finally (return i)))))

(defun build-path (start end map)
  (iter
    (with dir = (mapcar #'signum (map 'list #'- end start)))
    (for cur initially start then (map 'list #'+ cur dir))
    (setf (gethash cur map) t)
    (until (equal cur end))
    (finally (return map))))

(defun build-map (paths)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for path in paths)
    (iter
      (for start in path)
      (for end in (cdr path))
      (setf ret (build-path start end ret)))
    (finally (return ret))))

