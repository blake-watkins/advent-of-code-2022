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

;; for each direction, store its (r c) offset
(defparameter *direction-info*
  '((:right (0 1)) (:down (1 0)) (:left (0 -1)) (:up (-1 0))))

(defun turn (orientation direction)
  (labels ((turn-int (orientation direction)
             (let ((axis (ecase direction
                           ((:r :yaw-right) '(0 0 -1))
                           ((:l :yaw-left) '(0 0 1))
                           (:pitch-up '(0 -1 0))
                           (:pitch-down '(0 1 0))
                           (:roll-cw '(1 0 0))
                           (:roll-ccw '(-1 0 0)))))
               (q-compose orientation (q-rotor (/ pi 2) axis)))))
    (reduce #'turn-int
            (if (listp direction) direction (list direction))
            :initial-value orientation)))

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

(defun get-face-orientations (face-size current net face-orientations
                              &key (prev nil) (dir nil))
  (setf (gethash current face-orientations)
        (if dir
            (turn (gethash prev face-orientations)
                  (ecase dir
                    (:right :pitch-up)
                    (:left :pitch-down)
                    (:up :roll-cw)
                    (:down :roll-ccw)))
            '(1 0 0 0)))
  (iter
    (for (dir offset) in *direction-info*)
    (for neighbour = (point+ current offset))
    (when (and (valid-face-corners (point* face-size neighbour) face-size net)
               (or (null prev) (not (equal prev neighbour))))
      (get-face-orientations face-size neighbour net face-orientations
                            :prev current
                            :dir dir))
    (finally (return face-orientations))))

(defun rc-to-cube (rc face-orientation face-size)
  (let* ((half-length (/ (1- face-size) 2))
         (center (list half-length half-length (1+ half-length))))
    (destructuring-bind (r c) rc
      (q-round (point+ (q-rotate-vector
                        (point- (list c (- face-size r 1) 0) center)
                        face-orientation)
                       center)))))

;; Return a map of each cube position to the corresponding rc position in the net
(defun get-cube (face-size face-orientations net)
  (iter
    (with cube = (make-hash-table :test 'equal))
    (for (pos square) in-hashtable net)
    (when (or (eq square :open) (eq square :wall))
      (for tile = (mapcar (lambda (x) (floor x face-size)) pos))
      (for rc = (mapcar (lambda (x) (mod x face-size)) pos))
      (for orientation = (gethash tile face-orientations))
      (for cube-pos = (rc-to-cube rc orientation face-size))
      (setf (gethash cube-pos cube) pos))
    (finally (return cube))))

(defun find-original-dir (dir orig-pos face-size face-orientations)
  (let* ((face-tile (mapcar (lambda (x) (floor x face-size)) orig-pos))
         (face-orientation (gethash face-tile face-orientations))
         (orig-dir
           (q-round (q-rotate-vector dir (q-reciprocal face-orientation)))))
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
        (let ((next-orientation (turn orientation :pitch-up)))
          (list (point+ next-position (forward-vector next-orientation))
                next-orientation)))))

(defun traverse (net path)
  (iter
    (with (face-size first-face) = (get-first-face net))
    (with face-orientations = (get-face-orientations face-size first-face net
                                                  (make-hash-table :test 'equal)))
    (with cube = (get-cube face-size face-orientations net))
    (with position = (rc-to-cube '(0 0) '(1 0 0 0) face-size))
    (with orientation = '(1 0 0 0))
    (for instruction in path)
;;    (break)
    (if (numberp instruction)
        (iter
          (repeat instruction)
          (for (next-position next-orientation) =
               (next-position position orientation cube))
          (for next-square = (gethash (gethash next-position cube) net))
;;          (break)
          (until (eq next-square :wall))
          (setf position next-position)
          (setf orientation next-orientation))
        (setf orientation (turn orientation instruction)))
    (finally
     (return (password (gethash position cube)
                       (find-original-dir (forward-vector orientation)
                                          (gethash position cube)
                                          face-size
                                          face-orientations))))))

(defun day22 (input)
  (let ((parsed (run-parser (one-or-more (parse-until (parse-file))) input)))
    (iter
      (for (net path) in parsed)
      (sum (traverse net path)))))
