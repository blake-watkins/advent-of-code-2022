(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-list (parse-alphanumeric) "")))

(defun neighbours (square squares)
  (remove-if-not (lambda (s) (gethash s squares))
                 (mapcar (lambda (dir)
                           (map 'list #'+ square dir))
                         '((0 1) (0 -1) (1 0) (-1 0)))))

(defun get-map (parsed)
  (let ((map (hash-table-from-list-list parsed))
        (start nil)
        (end nil))
    (iter
      (for (vertex val) in-hashtable map)
      (when (char= val #\S) (setf start vertex))
      (when (char= val #\E) (setf end vertex)))
    (setf (gethash start map) #\a)
    (setf (gethash end map) #\z)    
    (list start end map)))

(defun start-squares (map)
  (iter
    (for (vertex val) in-hashtable map)
    (when (char= val #\a) (collect vertex))))

(defun day12 (input &key (part 1))
  (destructuring-bind (start end squares)
      (get-map (run-parser (parse-file) input))
    (labels ((neighbour-fn (cur)
               (let ((cur-elevation (char-code (gethash cur squares))))
                 (remove-if-not
                  (lambda (n)
                    (let ((n-elevation (char-code (gethash n squares))))
                      (<= n-elevation (1+ cur-elevation))))
                  (neighbours cur squares)))))
      (iter
        (for (vertex parent distance)
             in-bfs-from (if (= part 1) start (start-squares squares))
             neighbours #'neighbour-fn
             test 'equal)
        (until (equal vertex end))
        (finally (return distance))))))
