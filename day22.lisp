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

(defun q* (q1 q2)
  (destructuring-bind (a1 b1 c1 d1) (if (numberp q1) (list q1 0 0 0) q1)
    (destructuring-bind (a2 b2 c2 d2) (if (numberp q2) (list q2 0 0 0) q2)
      (list (+ (* a1 a2) (* -1 b1 b2) (* -1 c1 c2) (* -1 d1 d2))
            (+ (* a1 b2) (* b1 a2)    (* c1 d2)    (* -1 d1 c2))
            (+ (* a1 c2) (* -1 b1 d2) (* c1 a2)    (* d1 b2))
            (+ (* a1 d2) (* b1 c2)    (* -1 c1 b2) (* d1 a2))))))

(defun q-conjugate (q)
  (cons (first q) (mapcar #'- (cdr q))))

(defun q-norm (q)
  (sqrt (reduce #'+ q :key (lambda (x) (* x x)) :initial-value 0)))

(defun q-normalize (q)
  (q* (/ 1 (q-norm q)) q))

(defun q-reciprocal (q)
  (q* (/ 1 (expt (q-norm q) 2)) (q-conjugate q)))

(defun q-round (q)
  (mapcar #'round q))

(defun q-rotor (angle axis)
  (cons (cos (/ angle 2)) (mapcar (lambda (c) (* (sin (/ angle 2)) c)) axis)))

(defun q-rotate (vector rotor)
  (let ((rotor-1 (q-reciprocal rotor)))
    (cdr (q* rotor (q* (q-pure-q vector) rotor-1)))))



(defparameter *cube-rotation* '(1 0 0 0))

;; for each direction, store its (r c) offset, 
;; and a rotor to roll in that direction, and  the next direction clockwise,
(defparameter *direction-info*
  `((:right (0  1) ,(q-rotor (/ pi 2) '(0 -1 0)) :down)
    (:left  (0 -1) ,(q-rotor (/ pi 2) '(0  1 0)) :up)
    (:up    (-1 0) ,(q-rotor (/ pi 2) '( 1 0 0)) :right)
    (:down  ( 1 0) ,(q-rotor (/ pi 2) '(-1 0 0)) :left)))

(defun turn (dir l-r)
  (ecase l-r
    (:r (fourth (find dir *direction-info* :key #'first)))
    (:l (fourth (find dir *direction-info* :key #'first)))))

(defun direction-from (source target)
  (let ((diff (point- target source)))
    (first (find diff *direction-info* :test 'equal :key #'second))))

(defun direction-offset (dir)
  (second (find dir *direction-info* :key #'first)))

(defun direction-rotor (dir)
  (third (find dir *direction-info* :key #'first)))

(defun roll (rotor direction)
  (q* rotor (direction-rotor direction)))

(defun next-square-in-direction (rc dir tile-size rotation)
  (destructuring-bind (r c) (point+ rc (direction-offset dir))
    (let ((next-rc (list (mod r tile-size) (mod c tile-size)))
          (next-rotation
            (if (and (<= 0 r (1- tile-size)) (<= 0 c (1- tile-size)))
                rotation
                (roll rotation 
                      (cond
                        ((< r 0) :up) ((>= r tile-size) :down)
                        ((< c 0) :left) ((>= c tile-size) :right))))))
      (list next-rc next-rotation))))

;; Checks that the three other points on the square of size offset exist in the
;; map and that they're not blank.
(defun valid-tile-corners (pos offset map)
  (destructuring-bind (r c) pos
    (every (lambda (pos)
             (let ((square (gethash pos map)))
               (and square (not (eq :blank square)))))
           (list (list (+ r offset) c)
                 (list r (+ c offset))
                 (list (+ r offset) (+ c offset))))))

;; Find the tile size and first tile index rc by finding the first non-blank
;; square in the first row, then trying increasing square sizes until one of the
;; corners becomes invalid.
(defun get-tile-information (map)
  (let ((col (iter
               (for c from 0)
               (while (eq (gethash (list 0 c) map) :blank))
               (finally (return c)))))
    (iter
      (for tile-size from 1)
      (while (valid-tile-corners (list 0 col) tile-size map) )
      (finally (return (list tile-size (list 0 (floor col tile-size))))))))

;; Is a tile index RC a valid tile of size tile-size
(defun valid-tile (rc tile-size map)
  (valid-tile-corners (mapcar (lambda (x) (* x tile-size)) rc) (1- tile-size) map))

(defun get-cube (map)
  (destructuring-bind (tile-size first-tile) (get-tile-information map)
    (labels ((neighbours (pos)
               (iter
                 (for (nil offset) in *direction-info*)
                 (for neighbour = (point+ pos offset))
                 (when (valid-tile neighbour tile-size map)
                   (collect neighbour)))))
      (iter
        (with tile-rotations = (make-hash-table :test 'equal))
        (with cube = (make-hash-table :test 'equal))
        (for (tile parent distance) in-bfs-from first-tile
             neighbours #'neighbours
             test 'equal
             single t)
        (for tile-rotation =
             (if parent
                 (roll (gethash parent tile-rotations)
                       (direction-from parent tile))
                 '(1 0 0 0)))
        (setf (gethash tile tile-rotations) tile-rotation)
        (copy-tile-to-cube tile tile-size tile-rotation map cube)
        (finally (return (list cube tile-size tile-rotations)))))))

(defparameter *original-rc* (make-hash-table :test 'equal))

(defun copy-tile-to-cube (tile tile-size rotation map cube)
  (iter
    (with tile-offset-rc = (point* tile-size tile))
    (for r from 0 below tile-size)
    (iter
      (for c from 0 below tile-size)
      (for rc = (list r c))
      (for cube-pos = (rc-to-cube rc rotation tile-size))
      (for orig-pos = (point+ rc tile-offset-rc))
      (setf (gethash cube-pos cube) (gethash orig-pos map))
      (setf (gethash cube-pos *original-rc*) orig-pos))))

(defun rc-to-xyz (rc tile-size)
  (destructuring-bind (r c) rc
    (list c (- tile-size r 1) 0)))

(defun cube-center-xyz (tile-size)
  (let ((half-length (/ (1- tile-size) 2)))
    (list half-length half-length (1+ half-length))))

(defun rc-to-cube (rc cube-rotation tile-size)
  (let* ((center (cube-center-xyz tile-size))
         (xyz (point- (rc-to-xyz rc tile-size) center))
         (rotated-xyz (point+ (q-rotate xyz cube-rotation) center)))
    (mapcar #'round rotated-xyz)))

(defun day22 (input)
  (destructuring-bind (map path) (run-parser (parse-file) input)
    (iter
      (with (cube tile-size) = (get-cube map))
      (with rotation = '(1 0 0 0))
      (with pos = '(0 0))
      (with dir = :right)      
      (initially (setf *cube* cube))
      (for instr in path)
      (if (numberp instr)
          (iter
            (repeat instr)
            (for (next-pos next-rotation) =
                 (next-square-in-direction pos dir tile-size rotation))
            (let ((next-square
                    (gethash (rc-to-cube next-pos next-rotation tile-size) cube)))
              (unless (eq next-square :wall)
                (setf pos next-pos)
                (setf rotation next-rotation))))
          (setf dir (turn dir instr)))
      (finally (return (password
                        (gethash (rc-to-cube pos rotation tile-size)
                                 *original-rc*)
                        dir))))))



