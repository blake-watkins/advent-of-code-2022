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

(defparameter *direction-info*
  '((:right . ((0  1) 0 (0 0 -1))) (:down . (( 1 0) 1 (0  1 0)))
    (:left .  ((0 -1) 2 (0 0  1))) (:up .   ((-1 0) 3 (0 -1 0)))
    (:cw . (nil nil (1 0 0))) (:ccw . (nil nil (-1 0 0)))))

(defun offset-direction (offset)
  (car (rassoc offset *direction-info* :test 'equal :key #'first)))
(defun direction-offset (dir) (cadr (assoc dir *direction-info*)))
(defun direction-score (dir) (caddr (assoc dir *direction-info*)))
(defun direction-axis (dir) (cadddr (assoc dir *direction-info*)))

(defparameter *reference-frame* '(1 0 0 0))
(defparameter *down-frame* (turn (turn *reference-frame* :down) :ccw))

(defun forward-vector (frame)
  (q-round (q-rotate-vector '(1 0 0) frame)))

(defun turn (frame direction &key in-frame)
  (let ((rotor (q-rotor (/ pi 2) (direction-axis direction))))
    (q-compose frame
               (if in-frame
                   (q-compose in-frame (q-compose rotor (q-reciprocal in-frame)))
                   rotor))))

(defun get-face-frames (face-size current net face-frames
                              &key (prev nil) (frame *reference-frame*))
  (setf (gethash current face-frames) frame)
  (iter
    (for dir in '(:up :down :left :right))
    (for neighbour = (point+ current (direction-offset dir)))
    (when (and (valid-face-corners (point* face-size neighbour) face-size net)
               (or (null prev) (not (equal prev neighbour))))
      (for neighbour-frame = (turn frame dir :in-frame *down-frame*))
      (get-face-frames face-size neighbour net face-frames
                             :prev current
                             :frame neighbour-frame))
    (finally (return face-frames))))

(defun rc-to-cube (rc frame face-size)
  (destructuring-bind (r c) rc
    (let* ((half-length (/ (1- face-size) 2))
           (center (list half-length half-length (1+ half-length)))
           (xyz (list c (- face-size r 1) 0)))
      (q-round
       (point+ (q-rotate-vector (point- xyz center) frame) center)))))

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
  (let ((next-position (point+ position (forward-vector frame))))
    (if (gethash next-position cube)
        (list next-position frame)
        (let ((next-frame (turn frame :up)))
          (list (point+ next-position (forward-vector next-frame)) next-frame)))))

(defun find-net-dir (frame net-pos face-size face-frames)
  (let* ((face-tile (mapcar (lambda (x) (floor x face-size)) net-pos))
         (face-frame (gethash face-tile face-frames))
         (dir-xyz (forward-vector (q-compose (q-reciprocal face-frame) frame))))
    (destructuring-bind (x y) (subseq dir-xyz 0 2)
      (offset-direction (list (- y) x)))))

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos))) (* 4 (1+ (second pos))) (direction-score dir)))

(defun traverse (net path)
  (iter
    (with (face-size first-face) = (get-first-face net))
    (with face-frames = (get-face-frames face-size first-face net
                                         (make-hash-table :test 'equal)))
    (with cube = (get-cube face-size face-frames net))
    (with position = (rc-to-cube '(0 0) *reference-frame* face-size))
    (with frame = *reference-frame*)
    (for instruction in path)
    (if (numberp instruction)
        (iter
          (repeat instruction)
          (for (next-position next-frame) = (next-position position frame cube))
          (for next-square = (gethash (gethash next-position cube) net))
          (until (eq next-square :wall))
          (setf position next-position)
          (setf frame next-frame))
        (setf frame (turn frame instruction)))
    (finally
     (let* ((net-pos (gethash position cube))
            (net-dir (find-original-dir frame net-pos face-size face-frames)))
       (return (password net-pos net-dir))))))

(defun day22 (input)
  (let ((parsed (run-parser (one-or-more (parse-until (parse-file))) input)))
    (iter
      (for (net path) in parsed)
      (sum (traverse net path)))))
