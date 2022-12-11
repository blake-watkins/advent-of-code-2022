(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (with-monad
                 (assign dir (parse-keyword))
                 (parse-space)
                 (assign amount (parse-number))
                 (unit (list dir amount)))))

(defun move (pos dir)
  (destructuring-bind (r c) pos
    (ecase dir
      (:U (list (1- r) c))
      (:D (list (1+ r) c))
      (:L (list r (1- c)))
      (:r (list r (1+ c))))))

(defun update-tail (head tail)
  (let ((diff (map 'list #'- head tail)))
    (cond
      ((every (lambda (d) (<= (abs d) 1)) diff) tail)
      (t (map 'list #'+ tail
              (mapcar (lambda (d)
                        (if (= 0 d) 0 (/ d (abs d))))
                      diff))))))

(defun print-grid (head tail)
  (format nil "~{~a~}~%"
          (iter
            (for r from -4 to 0)
            (collect
                (format nil "~{~a~}~%"
                        (iter
                          (for c from 0 to 5)
                          (collect (cond
                                     ((equal (list r c) head) #\H)
                                     ((equal (list r c) tail) #\T)
                                     ((equal (list r c) '(0 0)) #\s)
                                     (t #\.)))))))))
(defun day9 (input)
  (let ((moves (run-parser (parse-file) input)))
    (iter
      (with head = '(0 0))
      (with tail = '(0 0))
      (with tail-positions = (make-hash-table :test 'equal))
      (for (direction amount) in moves)
      (iter
        (repeat amount)
        (setf head (move head direction))
        (setf tail (update-tail head tail))
        (setf (gethash tail tail-positions) t)
        (format t "~a" (print-grid head tail)))
      (finally (return (hash-table-count tail-positions))))))
