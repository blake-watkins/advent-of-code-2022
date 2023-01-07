(in-package :aoc-2022)

(defun parse-map ()
  (with-monad
    (parse-lines
     (one-or-more
      (with-monad
        (assign char (parse-character " .#"))
        (unit (case char (#\Space :blank) (#\. :open) (#\# :wall))))))))

(defun parse-path ()
  (one-or-more (either (parse-number) (parse-keyword #'upper-case-p))))

(defun parse-file ()
  (with-monad
    (assign map (parse-map))
    (parse-newline)
    (assign path (parse-until (parse-path)))
    (unit (list (hash-table-from-list-list map) path))))

;; for each direction, store its (r c) offset, 
;; and a rotor to roll in that direction, and  the next direction clockwise,
(defparameter *direction-info*
  `((:right (0  1) ,(q-rotor (/ pi 2) '(0 1 0)) :down)
    (:left  (0 -1) ,(q-rotor (/ pi 2) '(0 -1 0)) :up)
    (:up    (-1 0) ,(q-rotor (/ pi 2) '(-1 0 0)) :right)
    (:down  ( 1 0) ,(q-rotor (/ pi 2) '( 1 0 0)) :left)))

(defun turn (dir l-r)
  (ecase l-r
    (:r (fourth (find dir *direction-info* :key #'first)))
    (:l (first (find dir *direction-info* :key #'fourth)))))

(defun direction-from (source target)
  (let ((diff (point- target source)))
    (first (find diff *direction-info* :test 'equal :key #'second))))

(defun direction-offset (dir)
  (second (find dir *direction-info* :key #'first)))

(defun direction-rotor (dir)
  (third (find dir *direction-info* :key #'first)))

(defun roll (rotor direction)
  (q* (direction-rotor direction) rotor))

;; Checks that the four corners of the square of size OFFSET exist in the
;; map and that they're not blank.
(defun valid-face-corners (pos size map)
  (every (lambda (offset)
           (let* ((coord (point+ pos (point* size offset)))
                  (square (gethash coord map)))
             (and square (not (eq :blank square)))))
         '((0 0) (1 0) (0 1) (1 1))))

;; Is a face index RC a valid face of size face-size
(defun valid-face (rc face-size map)
  (valid-face-corners (point* face-size rc) (1- face-size) map))

;; Find the face size and first face index rc by finding the first non-blank
;; square in the first row, then trying increasing square sizes until one of the
;; corners becomes invalid.
(defun get-first-face (map)
  (let ((col (iter
               (for c from 0)
               (while (eq (gethash (list 0 c) map) :blank))
               (finally (return c)))))
    (iter
      (for face-size from 1)
      (while (valid-face-corners (list 0 col) face-size map))
      (finally (return (list face-size (list 0 (floor col face-size))))))))

(defun get-cube (map)
  (destructuring-bind (face-size first-face) (get-first-face map)
    (labels ((neighbours (pos)
               (iter
                 (for (nil offset) in *direction-info*)
                 (for neighbour = (point+ pos offset))
                 (when (valid-face neighbour face-size map)
                   (collect neighbour)))))
      (iter
        (with face-info = (make-hash-table :test 'equal))
        (with cube = (make-hash-table :test 'equal))
        (with origins = (make-hash-table :test 'equal))
        (for (face parent distance) in-bfs-from first-face
             neighbours #'neighbours
             test 'equal
             single t)
        (for face-rotation =
             (if parent
                 (roll (gethash parent face-info)
                       (direction-from parent face))
                 '(1 0 0 0)))
        (setf (gethash face face-info) face-rotation)
        (setf (gethash (rc-to-cube '(0 0) face-rotation face-size) origins)
              (point* face-size face))
        (copy-face-to-cube face face-size face-rotation cube)
        (finally (return (list cube face-size origins)))))))

(defun copy-face-to-cube (face face-size rotation cube)
  (iter
    (with face-offset-rc = (point* face-size face))
    (for r from 0 below face-size)
    (iter
      (for c from 0 below face-size)
      (for rc = (list r c))
      (for cube-pos = (rc-to-cube rc rotation face-size))
      (for orig-rc = (point+ rc face-offset-rc))
      (setf (gethash cube-pos cube) orig-rc))))

(defun rc-to-cube (rc cube-rotation face-size)
  (let* ((half-length (/ (1- face-size) 2))
         (center (list half-length half-length (1+ half-length))))
    (destructuring-bind (r c) rc
      (q-round (point+ (q-rotate-vector
                        (point- (list c (- face-size r 1) 0) center)
                        (q-reciprocal cube-rotation))
                       center)))))

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

(defun password (pos dir)
  (+ (* 1000 (1+ (first pos)))
     (* 4 (1+ (second pos)))
     (ecase dir (:right 0) (:down 1) (:left 2) (:up 3))))

(defun find-original-dir (dir rotation origins face-size)
  (iter
    (with clockwise-rotor = (q-rotor (/ PI 2) '(0 0 -1)))
    (for i from 0)
    (for twisted first rotation then (q* clockwise-rotor twisted))
    (for ret first dir then (turn ret :r))
    (for face-origin = (gethash (rc-to-cube '(0 0) twisted face-size) origins))
    (finding ret such-that face-origin)))

(defun day22 (input)
  (destructuring-bind (map path) (run-parser (parse-file) input)
    (iter
      (with (cube face-size origins) = (get-cube map))
      (with rotation = '(1 0 0 0))
      (with pos = '(0 0))
      (with dir = :right)      
      (for instr in path)
      (if (numberp instr)
          (iter
            (repeat instr)
            (for (next-pos next-rotation) =
                 (next-square-in-direction pos dir face-size rotation))
            (let ((next-square
                    (gethash (gethash (rc-to-cube next-pos
                                                  next-rotation
                                                  face-size)
                                      cube)
                             map)))
              (unless (eq next-square :wall)
                (setf pos next-pos)
                (setf rotation next-rotation))))
          (setf dir (turn dir instr)))
;;      (break)
      (finally (return (password (gethash (rc-to-cube pos rotation face-size)
                                          cube)
                                 (find-original-dir dir
                                                    rotation
                                                    origins
                                                    face-size)))))))



