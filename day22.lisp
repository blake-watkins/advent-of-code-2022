(in-package :aoc-2022)

(defun parse-square ()
  (with-monad
    (assign char (parse-character " .#"))
    (unit (case char (#\Space :blank) (#\. :open) (#\# :wall)))))

(defun parse-path ()
  (one-or-more (either (parse-number) (parse-keyword #'upper-case-p))))

(defun parse-file ()
  (with-monad
    (assign net (parse-lines (one-or-more (parse-square))))
    (assign path (parse-until (parse-path)))
    (unit (list (hash-table-from-list-list net) path))))

;; for each direction, store its (r c) offset and a rotor to roll in that direction
(defparameter *direction-info*
  `((:right (0 1) ,(q-rotor (/ pi 2) '(0 1 0)))
    (:down  (1 0) ,(q-rotor (/ pi 2) '(1 0 0)))
    (:left  (0 -1) ,(q-rotor (/ pi 2) '(0 -1 0)))
    (:up    (-1 0) ,(q-rotor (/ pi 2) '(-1 0 0)))))

(defun turn (dir l-r)
  (let* ((cur-idx (position dir *direction-info* :key #'first))
         (new-idx (mod (+ cur-idx (if (eq l-r :r) 1 -1))
                       (length *direction-info*))))
    (first (elt *direction-info* new-idx))))

(defun direction-from (source target)
  (let ((diff (point- target source)))
    (first (find diff *direction-info* :test 'equal :key #'second))))

(defun direction-offset (dir)
  (second (find dir *direction-info* :key #'first)))

(defun direction-rotor (dir)
  (third (find dir *direction-info* :key #'first)))

(defun roll (rotor direction)
  (q* (direction-rotor direction) rotor))

;; Checks that the four corners of the square with top-left coordinate POS and of
;; size SIZE exist in the net and that they're not blank.
(defun valid-face-corners (pos size net)
  (and (> size 0)
       (every (lambda (offset)
                (let* ((coord (point+ pos (point* (1- size) offset)))
                       (square (gethash coord net)))
                  (and square (not (eq :blank square)))))
              '((0 0) (1 0) (0 1) (1 1)))))

;; Find the face size and first face tile by finding the first non-blank square
;; in the first row, then trying increasing square sizes until one of the corners
;; becomes invalid.
(defun get-first-face (net)
  (let ((col (iter
               (for c from 0)
               (while (eq (gethash (list 0 c) net) :blank))
               (finally (return c)))))
    (iter
      (for face-size from 0)
      (while (valid-face-corners (list 0 col) (1+ face-size) net))
      (finally (return (list face-size (list 0 (floor col face-size))))))))

;; Starting from the first tile in the net, do a BFS to find all tiles in the
;; net. As the search moves from tile to tile, "roll" the cube in the appropriate
;; direction. Return a map of each of the tiles to the rotation of the cube
;; required to bring them to the base of the cube.
(defun get-face-rotations (face-size first-face-tile net)
  (labels ((neighbours (tile)
             (remove-if-not
              (lambda (neighbour)
                (valid-face-corners (point* face-size neighbour) face-size net))
              (mapcar (lambda (offset) (point+ tile offset))
                      '((1 0) (-1 0) (0 1) (0 -1))))))
    (iter
      (with face-rotations = (make-hash-table :test 'equal))
      (for (face-tile parent-tile distance) in-bfs-from first-face-tile
           neighbours #'neighbours
           test 'equal
           single t)
      (for face-rotation =
           (if parent-tile
               (roll (gethash parent-tile face-rotations)
                     (direction-from parent-tile face-tile))
               '(1 0 0 0)))
      (setf (gethash face-tile face-rotations) face-rotation)
      (finally (return face-rotations)))))

;; Given an rc position on the base of the cube and the cube's rotation, return
;; the cube coordinate that maps to that position under that rotation.
;; Steps: (1) Convert the rc position to xyz position 
;;        (2) Center the cube to the origin by subtracting the cube center
;;        (3) Apply the reciprocal rotation to find where this coordinate came from
;;        (4) Move the cube back by adding the center
;;        (5) Round the coordinates back to integers to fix floating point errors
(defun rc-to-cube (rc rotation face-size)
  (let* ((half-length (/ (1- face-size) 2))
         (center (list half-length half-length (1+ half-length))))
    (destructuring-bind (r c) rc
      (q-round (point+ (q-rotate-vector
                        (point- (list c (- face-size r 1) 0) center)
                        (q-reciprocal rotation))
                       center)))))

;; Return a list of all of the cube coordinates corresponding to the 0,0 rc
;; coordinate of each face. 
(defun get-face-origins (face-size face-rotations)
  (iter
    (for (nil face-rotation) in-hashtable face-rotations)
    (collect (rc-to-cube '(0 0) face-rotation face-size))))

;; Return a map of each cube position to the corresponding rc position in the net
(defun get-cube (face-size face-rotations net)
  (iter
    (with cube = (make-hash-table :test 'equal))
    (for (pos square) in-hashtable net)
    (when (or (eq square :open) (eq square :wall))
      (for tile = (mapcar (lambda (x) (floor x face-size)) pos))
      (for rc = (mapcar (lambda (x) (mod x face-size)) pos))
      (for rotation = (gethash tile face-rotations))
      (for cube-pos = (rc-to-cube rc rotation face-size))
      (setf (gethash cube-pos cube) pos))
    (finally (return cube))))

;; If at position RC in direction DIR, with a cube rotation ROTATION, find the
;; next position and rotation if you take one step. The position always stays
;; on the base of the cube, if we try to move off then roll the cube in the
;; appropriate direction. 
(defun next-square-in-direction (rc dir face-size rotation)
  (destructuring-bind (r c) (point+ rc (direction-offset dir))
    (let ((next-rc (list (mod r face-size) (mod c face-size)))
          (next-rotation
            (if (and (<= 0 r (1- face-size)) (<= 0 c (1- face-size)))
                rotation
                (roll rotation 
                      (cond
                        ((< r 0) :up) ((>= r face-size) :down)
                        ((< c 0) :left) ((>= c face-size) :right))))))
      (list next-rc next-rotation))))

;; After finishing, find the direction in terms of the original net. Twist the
;; cube clockwise while turning the direction right until the 0,0 coordinate
;; matches one of the original face 0,0 coordinates. 
(defun find-original-dir (dir rotation origins face-size)
  (iter
    (with clockwise-rotor = (q-rotor (/ PI 2) '(0 0 -1)))
    (for twisted first rotation then (q* clockwise-rotor twisted))
    (for orig-dir first dir then (turn orig-dir :r))
    (for correct-twist =
         (member (rc-to-cube '(0 0) twisted face-size) origins :test 'equal))
    (finding orig-dir such-that correct-twist)))

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos)))
     (* 4 (1+ (second pos)))
     (ecase dir (:right 0) (:down 1) (:left 2) (:up 3))))

(defun day22 (input)
  (destructuring-bind (net path) (run-parser (parse-file) input)
    (iter
      (with (face-size first-face) = (get-first-face net))
      (with face-rotations = (get-face-rotations face-size first-face net))
      (with origins = (get-face-origins face-size face-rotations))
      (with cube = (get-cube face-size face-rotations net))
      (with rotation = '(1 0 0 0))
      (with pos = '(0 0))
      (with dir = :right)      
      (for instr in path)
      (if (numberp instr)
          (iter
            (repeat instr)
            (for (next-pos next-rotation) =
                 (next-square-in-direction pos dir face-size rotation))
            (for cube-pos = (rc-to-cube next-pos next-rotation face-size))
            (for next-square = (gethash (gethash cube-pos cube) net))
            (until (eq next-square :wall))
            (setf pos next-pos)
            (setf rotation next-rotation))
          (setf dir (turn dir instr)))
      (finally
       (return (password (gethash (rc-to-cube pos rotation face-size) cube)
                         (find-original-dir dir rotation origins face-size)))))))



