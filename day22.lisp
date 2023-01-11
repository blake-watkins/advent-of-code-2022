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
  `((:right ( 0  1) ,(q-rotor (/ pi 2) '( 0  1 0)))
    (:down  ( 1  0) ,(q-rotor (/ pi 2) '( 1  0 0)))
    (:left  ( 0 -1) ,(q-rotor (/ pi 2) '( 0 -1 0)))
    (:up    (-1  0) ,(q-rotor (/ pi 2) '(-1  0 0)))))

(defun turn (orientation direction)
  (q-compose orientation (q-rotor (/ pi 2) (ecase direction
                                             (:r '(0 0 1))
                                             (:l '(0 0 -1))
                                             (:up '(0 -1 0))
                                             (:down '(0 1 0))))))

(defun roll (rotor direction)
  (q-compose (third (find direction *direction-info* :key #'first)) rotor))

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
      (for face-size from 1)
      (finding
       (list face-size (list 0 (floor col face-size)))
       such-that (not (valid-face-corners (list 0 col) (1+ face-size) net))))))

;; Starting from the first tile in the net, recursively find all tiles in the
;; net. As the search moves from tile to tile, "roll" the cube in the appropriate
;; direction. Return a map of each of the tiles to the rotation of the cube
;; required to bring them to the base of the cube.
(defun get-face-rotations (face-size current net face-rotations
                           &key (parent nil) (roll-dir nil))
  (setf (gethash current face-rotations)
        (if roll-dir (roll (gethash parent face-rotations) roll-dir) '(1 0 0 0)))
  (iter
    (for (dir offset) in *direction-info*)
    (for neighbour = (point+ current offset))
    (when (and (valid-face-corners (point* face-size neighbour) face-size net)
               (or (null parent) (not (equal parent neighbour))))
      (get-face-rotations face-size neighbour net face-rotations
                          :parent current
                          :roll-dir dir))
    (finally (return face-rotations))))

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

(defun find-original-dir (dir orig-pos face-size face-rotations)
  (let* ((face-tile (mapcar (lambda (x) (floor x face-size)) orig-pos))
         (face-rotation (gethash face-tile face-rotations))
         (orig-dir (q-round (q-rotate-vector dir face-rotation))))
    (destructuring-bind (x y) (subseq orig-dir 0 2)
      (first (find (list (- y) x) *direction-info* :key #'second :test 'equal)))))

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos)))
     (* 4 (1+ (second pos)))
     (ecase dir (:right 0) (:down 1) (:left 2) (:up 3))))

(defun forward-vector (orientation)
  (q-round (q-rotate-vector '(1 0 0) orientation)))

(defun next-position (position orientation cube)
  (let ((next-position (point+ position (forward-vector orientation))))
    (if (gethash next-position cube)
        (list next-position orientation)
        (let ((next-orientation (turn orientation :down)))
          (list (point+ next-position (forward-vector next-orientation))
                next-orientation)))))

(defun traverse (net path)
  (iter
    (with (face-size first-face) = (get-first-face net))
    (with face-rotations = (get-face-rotations face-size first-face net
                                               (make-hash-table :test 'equal)))
    (with cube = (get-cube face-size face-rotations net))
    (with position = (rc-to-cube '(0 0) '(1 0 0 0) face-size))
    (with orientation = (roll (roll '(1 0 0 0) :down) :down))
    (for instruction in path)
;;    (break)
    (if (numberp instruction)
        (iter
          (repeat instruction)
          (for (next-position next-orientation) =
               (next-position position orientation cube))
          (for next-square = (gethash (gethash next-position cube) net))
          (until (eq next-square :wall))
          (setf position next-position)
          (setf orientation next-orientation))
        (setf orientation (turn orientation instruction)))
    (finally
     (return (password (gethash position cube)
                       (find-original-dir (forward-vector orientation)
                                          (gethash position cube)
                                          face-size
                                          face-rotations))))))

(defun day22 (input)
  (let ((parsed (run-parser (one-or-more (parse-until (parse-file))) input)))
    (iter
      (for (net path) in parsed)
      (sum (traverse net path)))))
