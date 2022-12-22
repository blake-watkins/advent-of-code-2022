(in-package :aoc-2022)

(defun parse-map ()
  (with-monad
   (assign parsed
           (parse-lines
            (zero-or-more (either
                           (then (parse-character #\Space) (unit :blank))
                           (then (parse-character #\.) (unit :open))
                           (then (parse-character #\#) (unit :wall))))))
    (unit parsed)))

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

(defparameter *teleports*
  '(((0 0) :up ((:l :f) (:l :l) ((0 2) :down (:f :l) (:f :f))))
    ((0 1) :up ((:l :f) (:l :l) ((0 2) :right (:f :f) (:l :f))))
    ((0 1) :left ((:l :l) (:f :l) ((1 1) :down (:f :l) (:f :f))))
    ((-1 2) :up ((:l :f) (:l :l) ((1 0) :down (:f :l) (:f :f))))
    ((0 3) :right ((:f :f) (:l :f) ((2 3) :left (:l :l) (:f :l))))
    ((1 3) :right ((:f :f) (:l :f) ((2 3) :down (:f :l) (:f :f))))
    ((1 3) :up ((:l :f) (:l :l) ((1 2) :left (:l :l) (:f :l))))
    ((2 4) :right ((:f :f) (:l :f) ((0 2) :left (:l :l) (:l :f))))
    ((3 3) :down ((:f :f) (:f :l) ((2 2) :up (:l :l) (:l :f))))
    ((3 2) :down ((:f :f) (:f :l) ((1 0) :up (:l :l) (:l :f))))
    ((2 1) :left ((:f :l) (:l :l) ((1 1) :up (:l :l) (:l :f))))
    ((2 1) :down ((:f :f) (:f :l) ((2 2) :right (:l :f) (:f :f))))
    ((2 0) :down ((:f :f) (:f :l) ((2 2) :up (:l :l) (:l :f))))
    ((1 -1) :left ((:f :l) (:l :l) ((2 3) :up (:l :l) (:l :f))))))

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
         (teleport (iter
                     (for (t-tile t-dir teleport) in *teleports*)
                     (finding teleport such-that (and (equal tile t-tile)
                                                      (eq dir t-dir))))))
    (destructuring-bind (r1-s r1-e (to-tile to-dir r2-s r2-e)) teleport
      (list (map-range pos
                       (translate r1-s tile multiplier)
                       (translate r1-e tile multiplier)
                       (translate r2-s to-tile multiplier)
                       (translate r2-e to-tile multiplier))
            to-dir))))

(defun off-grid (pos map)
  (let ((square (gethash pos map)))
    (or (null square) (eq :blank square))))

(defun move-one (pos dir map)
  (let ()

))

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
