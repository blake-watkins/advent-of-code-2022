(in-package :aoc-2022)

(defun parse-square ()
  (with-monad
    (assign char (parse-character " .#"))
    (unit (case char (#\Space :blank) (#\. :open) (#\# :wall)))))

(defun parse-direction ()
  (with-monad
    (assign dir (parse-keyword #'upper-case-p))
    (unit (ecase dir (:l :left) (:r :right)))))

(defun parse-path ()
  (one-or-more (either (parse-number) (parse-direction))))

(defun parse-file ()
  (with-monad
    (assign net (parse-lines (one-or-more (parse-square))))
    (assign path (parse-until (parse-path)))
    (unit (list (hash-table-from-list-list net) path))))

(defun valid-face-corners (pos size net)
  (and (> size 0)
       (every (lambda (offset)
                (let* ((coord (point+ pos (point* (1- size) offset)))
                       (square (gethash coord net)))
                  (and square (not (eq :blank square)))))
              '((0 0) (1 0) (0 1) (1 1)))))

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

(defparameter *reference-frame* '(1 0 0 0))
(defparameter *reference-frame-forward* '(0 0 -1))

(defparameter *direction-info*
    '((:right (0  1) 0 (0 -1 0)) (:down ( 1 0) 1 (-1 0 0))
      (:left  (0 -1) 2 (0  1 0)) (:up   (-1 0) 3 ( 1 0 0))
      (:cw nil nil (0 0 -1)) (:ccw nil nil (0 0 1))))

(defun direction-offset (dir) (cadr (assoc dir *direction-info*)))
(defun direction-score (dir) (caddr (assoc dir *direction-info*)))
(defun direction-axis (dir) (cadddr (assoc dir *direction-info*)))
(defun offset-direction (offset)
  (car (rassoc offset *direction-info* :test 'equal :key #'first)))

(defun turn (frame direction)
  (q-compose frame (q-rotor (/ pi 2) (direction-axis direction))))

(defun frame-forward (frame)
  (q-round (q-rotate-vector *reference-frame-forward* frame)))

(defun rc-to-cube (rc frame face-size)
  (destructuring-bind (r c) rc
    (let* ((half-length (/ (1- face-size) 2))
           (center (list half-length half-length (1+ half-length)))
           (xyz (list c (- face-size r 1) 0)))
      (q-round
       (point+ (q-rotate-vector (point- xyz center) frame) center)))))

(defun get-face-frames (face-size current net
                        &key (face-frames (make-hash-table :test 'equal))
                             (prev nil)
                             (current-frame *reference-frame*))
  (setf (gethash current face-frames) current-frame)
  (iter
    (for direction in '(:up :down :left :right))
    (for neighbour = (point+ current (direction-offset direction)))
    (when (and (valid-face-corners (point* face-size neighbour) face-size net)
               (or (null prev) (not (equal prev neighbour))))
      (for neighbour-frame = (turn current-frame direction))
      (get-face-frames face-size neighbour net
                       :face-frames face-frames
                       :prev current
                       :current-frame neighbour-frame))
    (finally (return face-frames))))

(defun get-cube (face-size face-frames net)
  (iter
    (with cube = (make-hash-table :test 'equal))
    (for (pos square) in-hashtable net)
    (when (or (eq square :open) (eq square :wall))
      (for tile = (mapcar (lambda (x) (floor x face-size)) pos))
      (for rc = (mapcar (lambda (x) (mod x face-size)) pos))
      (for frame = (gethash tile face-frames))
      (for cube-pos = (rc-to-cube rc frame face-size))
      (setf (gethash cube-pos cube) pos))
    (finally (return cube))))

(defun next-position (position frame cube)
  (let ((next-position (point+ position (frame-forward frame))))
    (if (gethash next-position cube)
        (list next-position frame)
        (let ((next-frame (turn frame :up)))
          (list (point+ next-position (frame-forward next-frame)) next-frame)))))

(defun find-net-direction (frame face-frame)
  (destructuring-bind (x y)
      (subseq (frame-forward (q-compose (q-reciprocal face-frame) frame)) 0 2)
    (offset-direction (list (- y) x))))

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos))) (* 4 (1+ (second pos))) (direction-score dir)))

(defun traverse (net path)
  (iter
    (with (face-size first-face) = (get-first-face net))
    (with face-frames = (get-face-frames face-size first-face net))
    (with cube = (get-cube face-size face-frames net))
    (with position = (rc-to-cube '(0 0) *reference-frame* face-size))
    (with frame = (turn (turn *reference-frame* :right) :cw))
    (for steps-or-direction in path)
    (if (numberp steps-or-direction)
        (iter
          (repeat steps-or-direction)
          (for (next-position next-frame) = (next-position position frame cube))
          (for next-square = (gethash (gethash next-position cube) net))
          (until (eq next-square :wall))
          (setf position next-position)
          (setf frame next-frame))
        (setf frame (turn frame steps-or-direction)))
    (finally
     (let* ((net-position (gethash position cube))
            (face-tile (mapcar (lambda (x) (floor x face-size)) net-position))
            (face-frame (gethash face-tile face-frames))
            (net-direction (find-net-direction frame face-frame)))
       (return (password net-position net-direction))))))

(defun day22 (input)
  (let ((parsed (run-parser (one-or-more (parse-until (parse-file))) input)))
    (iter
      (for (net path) in parsed)
      (sum (traverse net path)))))
