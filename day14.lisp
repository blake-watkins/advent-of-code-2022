(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-list (parse-number-list) " -> ")))

(defun build-path (start end map)
  (iter
    (with dir = (point-signum (point- end start)))
    (for cur initially start then (point+ cur dir))
    (setf (gethash cur map) t)
    (until (equal cur end))
    (finally (return map))))

(defun build-map (paths)
  (iter
    (with map = (make-hash-table :test 'equal))
    (for path in paths)
    (reduce (lambda (s e) (build-path s e map) e) path)
    (finally (return map))))

(defun move-sand (sand occupied-fn)
  (let ((falling (iter
                   (for dir in '((0 1) (-1 1) (1 1)))
                   (for next = (point+ sand dir))
                   (finding next such-that (not (funcall occupied-fn next))))))
    (if falling
        (list :falling falling)
        (list :landed sand))))

(defun day14 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (map (build-map parsed))
         (floor (+ 2 (iter
                       (for (pos nil) in-hashtable map)
                       (maximizing (second pos)))))
         (occupied
           (lambda (sand)
             (or (gethash sand map)
                 (when (= part 2) (= (second sand) floor))))))
    (iter
      (with start = '(500 0))
      (for num-units from 0)
      (until (funcall occupied start)) ; break for part 2
      (for status =
           (iter
             (initially (setf pos start))
             (for (status pos) next (move-sand pos occupied))
             (when (eq status :landed)
               (setf (gethash pos map) t))
             (until (or (eq status :landed)
                        (> (second pos) floor))) ; break for part 1
             (finally (return status))))
      (until (eq :falling status))
      (finally (return num-units)))))

;; This version is approx 20x faster for part 2 - just recursively see where the
;; sand goes. 
(defun day14-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (map (build-map parsed))
         (floor (+ 2 (iter
                       (for (pos nil) in-hashtable map)
                       (maximizing (second pos))))))
    (labels ((occupied (sand)
               (or (gethash sand map)
                   (= (second sand) floor)))
             (count-sand (from)
               (cond
                 ((occupied from) 0)
                 (t
                  (setf (gethash from map) t)
                  (1+ (iter
                        (for dir in '((0 1) (-1 1) (1 1)))
                        (for next-square = (point+ from dir))
                        (sum (count-sand next-square))))))))
      (count-sand '(500 0)))))

