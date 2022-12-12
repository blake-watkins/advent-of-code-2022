(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-list (parse-alphanumeric) "")))

(defun neighbours (cur squares)
  (remove-if-not (lambda (s)
                   (let ((from-char (gethash s squares)))
                     (and from-char
                          (<= (char-code (gethash cur squares))
                              (1+ (char-code from-char))))))
                 (mapcar (lambda (dir) (map 'list #'+ cur dir))
                         '((0 1) (0 -1) (1 0) (-1 0)))))

(defun get-map (parsed)
  (iter
    (with start = nil)
    (with end = nil)
    (with squares = (make-hash-table :test 'equal))
    (for r from 0)
    (for row in parsed)
    (iter
      (for c from 0)
      (for square in row)
      (setf (gethash (list r c) squares) square)
      (when (char= square #\S)
        (setf start (list r c))
        (setf (gethash (list r c) squares) #\a))
      (when (char= square #\E)
        (setf end (list r c))
        (setf (gethash (list r c) squares) #\z)))
    (finally (return (list start end squares)))))

(defun day12 (input)
  (let* ((parsed (run-parser (parse-file) input)))
    (destructuring-bind (start end squares) (get-map parsed)
      (labels ((neighbour-fn (cur) (neighbours cur squares)))
        (iter
          (for (vertex parent distance ign)
               in-bfs-from end
               neighbours #'neighbour-fn
               test 'equal)
          (format t "~a~%" vertex)
          (until (char= (gethash vertex squares) #\a))
          (finally (return distance)))))))
