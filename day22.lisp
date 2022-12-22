(in-package :aoc-2022)

(defun parse-map (multiplier)
  (with-monad
   (assign parsed
           (parse-lines
            (zero-or-more (either
                           (then (parse-character #\Space) (unit :blank))
                           (then (parse-character #\.) (unit :open))
                           (then (parse-character #\#) (unit :wall))))))
    (unit (iter
            (with ret = (make-hash-table :test 'equal))
            (for r-idx from 0)
            (while (<= (* (1+ (floor r-idx multiplier)) multiplier) (length parsed)))
            (for r in parsed)
            
            (iter
              (for c-idx from 0)
              (while (<= (* (1+ (floor c-idx multiplier)) multiplier) (length r)))
              (setf (gethash (list r-idx c-idx) ret) (elt r c-idx)))
            (finally (return ret))))))

(defun parse-path ()
  (zero-or-more (either (parse-number) (parse-keyword #'upper-case-p))))

(defun parse-file ()
  (with-monad
    (assign map (parse-map 50))
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

(defparameter *range-info*
  '((:up ((:l :f) (:l :l)))
    (:down ((:f :f) (:f :l)))
    (:left ((:f :l) (:l :l)))
    (:right ((:f :f) (:l :f)))))

(defparameter *teleports*
  '(((-1 1) :up ((3 0) :right))
    ((-1 2) :up ((3 0) :up))
    ((0 3) :right ((2 1) :left :flipped))
    ((1 2) :down ((1 1) :left))
    ((1 2) :right ((0 2) :up))
    ((2 2) :right ((0 2) :left :flipped))
    ((3 1) :down ((3 0) :left))
    ((3 1) :right ((2 1) :up))
    ((4 0) :down ((0 2) :down))
    ((3 -1) :left ((0 1) :down))
    ((2 -1) :left ((0 1) :right :flipped))
    ((1 0) :up ((1 1) :right))
    ((1 0) :left ((2 0) :down))
    ((0 0) :left ((2 0) :right :flipped))))

(defun map-range (pos r1-s r1-e r2-s r2-e)
  (iter 
    (with r1-dir = (point-signum (point- r1-e r1-s)))
    (with r2-dir = (point-signum (point- r2-e r2-s)))
    (for from initially r1-s then (point+ from r1-dir))
    (for to initially r2-s then (point+ to r2-dir))
    (until (equal pos from))
    (finally (return (when (equal pos from) to)))))

(defun translate (l-f-pos tile multiplier)
  (map 'list
       (lambda (l-f tile-coord)
         (+ (* tile-coord multiplier) (if (eq :f l-f) 0 (1- multiplier))))
       l-f-pos
       tile))

(defun teleport (pos dir multiplier)
  (let* ((tile (mapcar (lambda (c) (floor c multiplier)) pos))
         (teleport 
           (iter
             (for (from-tile from-dir teleport) in *teleports*)
             (finding teleport such-that (and (equal tile from-tile)
                                              (eq dir from-dir))))))
    (destructuring-bind (to-tile to-dir &optional flipped?) teleport
      (destructuring-bind (r1-s r1-e) (second (find dir *range-info* :key #'first))
        (destructuring-bind (r2-s r2-e)
            (let ((to-info (second (find to-dir *range-info* :key #'first))))
              (if flipped? (reverse to-info) to-info))
          (list (map-range pos
                           (translate r1-s tile multiplier)
                           (translate r1-e tile multiplier)
                           (translate r2-s to-tile multiplier)
                           (translate r2-e to-tile multiplier))
                to-dir))))))

(defun off-grid (pos map)
  (let ((square (gethash pos map)))
    (or (null square) (eq :blank square))))

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
            (for next-pos = (move-in-direction pos dir))
            (for next-dir = dir)
            (when (off-grid next-pos map)
              (destructuring-bind (new-pos new-dir) (teleport next-pos dir 50)
                (setf next-pos new-pos)
                (setf next-dir new-dir)))
            (let ((next-square (gethash next-pos map)))
              (unless (eq next-square :wall)
                (setf pos next-pos)
                (setf dir next-dir))))
          (setf dir (turn dir instr)))
      (finally (return (password pos dir))))))
