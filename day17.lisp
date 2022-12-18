(in-package :aoc-2022)

(defun parse-file ()
  (zero-or-more (either (then (parse-character #\<) (unit :left))
                        (then (parse-character #\>) (unit :right)))))

(defparameter *shapes*
  '(((0 0) (0 1) (0 2) (0 3))
    ((0 1) (1 0) (1 1) (1 2) (2 1))
    ((0 0) (0 1) (0 2) (1 2) (2 2))
    ((0 0) (1 0) (2 0) (3 0))
    ((0 0) (1 0) (0 1) (1 1))))

(defun check-rock (pos rock map)
  (let ((abs-rock (mapcar (lambda (p) (point+ p pos)) rock)))
    (cond
      ((some (lambda (p) (not (<= 0 (second p) 6))) abs-rock) :intersect-wall)
      ((or (some (lambda (p) (< (first p) 0)) abs-rock)
           (some (lambda (p) (gethash p map)) abs-rock))
       :intersect-floor)
      (t :falling))))

(defun move-rock (jet rock pos map)
  (let ((shifted-pos (point+ pos (if (eq jet :left) '(0 -1) '(0 1)))))
    (when (eq :falling (check-rock shifted-pos rock map))
      (setf pos shifted-pos))
    (let ((fallen-pos (point+ pos '(-1 0))))
      (if (eq :falling (check-rock fallen-pos rock map))
          (list :falling fallen-pos)
          (list :intersect-floor pos)))))

(defun update-map (rock pos map)
  (iter
    (for stone in rock)
    (for abs-stone = (point+ stone pos))
    (setf (gethash abs-stone map) t)
    (maximizing (first abs-stone) into floor)
    (finally (return floor))))

(defparameter *cache* (make-hash-table :test 'equal))

(defun print-map (map height)
  (iter
    (repeat 10)
    (for r downfrom height)
    (format t "~{~a~}~%"
            (iter
              (for c from 0 below 7)
              (collect (if (gethash (list r c) map) #\# #\.))))))

(defun day17 (n input)
  (let ((parsed (run-parser (parse-file) input))
        (map (make-hash-table :test 'equal))
        (highest 0))
    (iter outer
      (repeat n)
      (with jet-idx = 0)
      (for i from 0)
      (generate jet next (let ((cur jet-idx))
                           (incf jet-idx)
                           (elt parsed (mod cur (length parsed)))))
      (for rock = (elt *shapes* (mod i (length *shapes*))))
      (for rock-pos = (list (+ 3 highest) 2))
      (iter
        (for (state pos) = (move-rock (in outer (next jet)) rock rock-pos map))
        (setf rock-pos pos)
        (until (eq :intersect-floor state))
        (finally
         (setf highest (max highest (1+ (update-map rock rock-pos map))))))
      (finally (return-from outer highest)))))



