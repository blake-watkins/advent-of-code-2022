(in-package :aoc-2022)

(defun parse-map ()
  (parse-lines (one-or-more (parse-character "#.><^v"))))

;; Return the start and end squares rc, the dimensions of the blizzard area, and a
;; hash keyed by position of each blizzard 
(defun get-map (input)
  (let* ((parsed (run-parser (parse-map) input))
         (start (list -1 (1- (position #\. (first parsed)))))
         (end (list (- (length parsed) 2)
                    (1- (position #\. (first (last parsed)))))))
    (iter
      (with blizzards = (make-hash-table :test 'equal))
      (for r from 1 below (1- (length parsed)))
      (for row = (elt parsed r))
      (iter
        (for c from 1 below (1- (length row)))
        (for square = (elt row c))
        (when (char/= square #\.)
          (setf (gethash (list (1- r) (1- c)) blizzards) square)))
      (finally (return (list start
                             end
                             (list (- (length parsed) 2)
                                   (- (length (first parsed)) 2))
                             blizzards))))))

(defun get-occupied-horizontal (rc dimensions blizzards)
  (iter
    (with (cur-row cur-col) = rc)
    (with width = (second dimensions))
    (for c below width)
    (for blizzard = (gethash (list cur-row c) blizzards))
    (when (and blizzard (or (char= blizzard #\>) (char= blizzard #\<)))
      (collect (list (mod (if (char= blizzard #\>)
                              (- cur-col c)
                              (- c cur-col))
                          width)
                     width)))))

(defun get-occupied-vertical (rc dimensions blizzards)
  (iter
    (with (cur-row cur-col) = rc)
    (with height = (first dimensions))
    (for r below height)
    (for blizzard = (gethash (list r cur-col) blizzards))
    (when (and blizzard (or (char= blizzard #\v) (char= blizzard #\^)))
      (collect (list (mod (if (char= blizzard #\v)
                              (- cur-row r)
                              (- r cur-row))
                          height)
                     height)))))

(defun get-occupied (dimensions blizzards)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (with (rows cols) = dimensions)
    (for r below rows)
    (iter
      (for c below cols)
      (for occupied =
           (concatenate 'list
                        (get-occupied-horizontal (list r c) dimensions blizzards)
                        (get-occupied-vertical (list r c) dimensions blizzards)))
      (setf (gethash (list r c) ret) occupied))
    (finally (return ret))))

(defun is-occupied-at (square time occupied)
  (some (lambda (occupied-at)
          (destructuring-bind (offset modulus) occupied-at
            (= (mod time modulus) offset)))
        (gethash square occupied)))

(define-condition found-end (condition)
  ((distance :initarg :distance)))

(defun find-path (start start-time end occupied)
  (labels
      ((vertex-fn (cur parent distance)
         (declare (ignore parent distance))
         (when (equal (first cur) end)
           (signal 'found-end :distance (second cur))))
       (neighbour-fn (vertex)
         (destructuring-bind (pos time) vertex
           (mapcar
            (lambda (square) (list (list square (1+ time)) (1+ time)))
            (remove-if-not
             (lambda (next-square)
               (or (equal next-square end)
                   (equal next-square start)
                   (and (nth-value 1 (gethash next-square occupied))
                        (not (is-occupied-at next-square (1+ time) occupied)))))
             (mapcar
              (lambda (offset) (point+ offset pos))
              '((-1 0) (1 0) (0 -1) (0 1) (0 0)))))))
       (heuristic-fn (vertex)
         (manhattan (first vertex) end)))
    (handler-case
        (a-star (list start start-time) #'vertex-fn #'neighbour-fn #'heuristic-fn)
      (found-end (fe) (slot-value fe 'distance)))))

(defun day24 (input &key (part 1))
  (destructuring-bind (start end dimensions blizzards) (get-map input)
    (let* ((occupied (get-occupied dimensions blizzards))
           (to-end-time (find-path start 0 end occupied)))
      (if (= part 1)
          to-end-time
          (let* ((to-start-time (find-path end to-end-time start occupied))
                 (to-end-again-time (find-path start to-start-time end occupied)))
            to-end-again-time)))))
