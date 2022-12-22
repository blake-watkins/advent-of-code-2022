(in-package :aoc-2022)

(defun parse-map ()
  (with-monad
   (assign parsed
           (parse-lines
            (zero-or-more (either
                           (then (parse-character #\Space) (unit :blank))
                           (then (parse-character #\.) (unit :open))
                           (then (parse-character #\#) (unit :wall))))))
    (unit (hash-table-from-list-list parsed))))

(defun parse-path ()
  (zero-or-more (either (parse-number) (parse-keyword #'upper-case-p))))

(defun parse-file ()
  (with-monad
    (assign map (parse-map))
    (assign path (parse-until (parse-path)))
    (unit (list map path))))

(defun opposite-direction (dir)
  (ecase dir
    (:left :right)
    (:right :left)
    (:up :down)
    (:down :up)))

(defun turn (dir l-r)
  (ecase l-r
    (:r (ecase dir (:left :up) (:up :right) (:right :down) (:down :left)))
    (:l (ecase dir (:left :down) (:down :right) (:right :up) (:up :left)))))

(defun move-in-direction (pos dir)
  (point+ pos (ecase dir
                (:left '(0 -1))
                (:right '(0 1))
                (:up '(-1 0))
                (:down '(1 0)))))

(defun first-in-direction (pos dir map)
  (iter
    (for cur initially pos then (move-in-direction cur dir))
    (for square = (gethash cur map) )
    (while square)
    (until (not (eq square :blank)))
    (finally (return (when (and square
                                (eq square :open))
                       cur)))))

(defun last-in-direction (pos dir map)
  (iter
    (for cur initially pos then next-pos)
    (for next-pos = (move-in-direction cur dir))
    (for next-square = (gethash next-pos map))
    (while next-square)
    (until (eq next-square :blank))
    (finally (return (when (or (not next-square)
                               (eq next-square :blank))
                       cur)))))

(defun off-grid (pos map)
  (let ((square (gethash pos map)))
    (or (null square) (eq :blank square))))

(defun move-one (pos dir map)
  (let ((next-pos (move-in-direction pos dir)))
    (when (off-grid next-pos map)
      (setf next-pos (last-in-direction pos (opposite-direction dir) map)))
    (let ((next-square (gethash next-pos map)))
      (if (eq next-square :wall)
          pos
          next-pos))))

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos)))
     (* 4 (1+ (second pos)))
     (ecase dir (:right 0) (:down 1) (:left 2) (:up 3))))

(defun day22 (input)
  (destructuring-bind (map path) (run-parser (parse-file) input)
    (iter
      (with pos = (first-in-direction '(0 0) :right map))
      (with dir = :right)      
      (for instr in path)
      (if (numberp instr)
          (iter
            (repeat instr)
            (setf pos (move-one pos dir map)))
          (setf dir (turn dir instr)))
      (finally (return (password pos dir))))))
