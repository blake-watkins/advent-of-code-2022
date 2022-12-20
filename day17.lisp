(in-package :aoc-2022)

(defun parse-file ()
  (with-monad
    (assign chars (zero-or-more (parse-character "<>")))
    (unit (mapcar (lambda (c) (case c (#\< :left) (#\> :right))) chars))))

(defparameter *shapes*
  '(((0 0) (0 1) (0 2) (0 3))
    ((0 1) (1 0) (1 1) (1 2) (2 1))
    ((0 0) (0 1) (0 2) (1 2) (2 2))
    ((0 0) (1 0) (2 0) (3 0))
    ((0 0) (1 0) (0 1) (1 1))))

(defun check-rock (pos rock map)
  (let ((abs-rock (mapcar (lambda (p) (point+ p pos)) rock)))
    (cond
      ((some (lambda (p) (not (<= 0 (second p) 6))) abs-rock) :intersect-wall)
      ((or (some (lambda (p) (< (first p) 0)) abs-rock)
           (some (lambda (p) (gethash p map)) abs-rock))
       :intersect-floor)
      (t :falling))))

(defun move-rock (jet rock pos map)
  (let ((shifted-pos (point+ pos (if (eq jet :left) '(0 -1) '(0 1)))))
    (when (eq :falling (check-rock shifted-pos rock map))
      (setf pos shifted-pos))
    (let ((fallen-pos (point+ pos '(-1 0))))
      (if (eq :falling (check-rock fallen-pos rock map))
          (list :falling fallen-pos)
          (list :intersect-floor pos)))))

(defun update-map (rock pos map)
  (iter
    (for stone in rock)
    (for abs-stone = (point+ stone pos))
    (setf (gethash abs-stone map) t)
    (maximizing (first abs-stone) into floor)
    (finally (return floor))))

;; Create a function that can be called with values. The function will return NIL
;; until it detects a repeat of length N in the stream of values. Once it does
;; it will return a two element list with the indices of the repeats. 
(defun make-sliding-window (n)
  (let ((history (fset:empty-map))
        (window (fset:empty-seq))
        (idx 0))
    (lambda (val)
      (let* ((added-window (fset:with-last window val))
             (new-window (if (<= (fset:size added-window) n)
                             added-window
                             (fset:less-first added-window)))
             (ret (when (fset:domain-contains? history new-window)
                    (list (1+ (- (fset:lookup history new-window) n))
                          (1+ (-  idx n))))))
        (setf window new-window)
        (setf history (fset:with history new-window idx))
        (incf idx)
        ret))))

;; Return either the height of the tower after N rocks have fallen if given N,
;; otherwise return a list of two indices where the pattern of height differences
;; repeats.
(defun get-height-or-repeat (parsed &key n window)
  (iter outer
    (with map = (make-hash-table :test 'equal))
    (with highest = 0)
    
    (generate jet-idx from 0)
    (generate jet next (elt parsed (mod (next jet-idx) (length parsed))))

    (for rock-idx from 0)
    (for rock = (elt *shapes* (mod rock-idx (length *shapes*))))
    (for rock-pos = (list (+ 3 highest) 2))

    (when (and n (= rock-idx n)) (terminate)) ;; break after n rocks, if given n.
    
    (iter
      (for (state pos) = (move-rock (in outer (next jet)) rock rock-pos map))
      (setf rock-pos pos)
      (until (eq :intersect-floor state)))
      
    (for rock-height = (1+ (update-map rock rock-pos map)))
    (for height-diff = (- rock-height highest))
    (setf highest (max highest rock-height))

    (when window
      (for repeat? = (funcall window height-diff)) 
      (until repeat?)) ;; break when there's a repeat, if given a sliding window
    
    (finally (return-from outer (if n highest repeat?)))))

(defun calc-height-from-repeat (idx1 idx2 input)
  (let* ((n 1000000000000)
         (modulus (- idx2 idx1))
         (height1 (get-height-or-repeat input :n idx1))
         (height2 (get-height-or-repeat input :n idx2))
         (height-diff (- height2 height1)))
    (multiple-value-bind (num-repeats offset) (floor (- n idx1) modulus)
      (+ (get-height-or-repeat input :n (+ idx1 offset))
         (* num-repeats height-diff)))))

(defun day17 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (if (= part 1)
        (get-height-or-repeat parsed :n 2022)
        (destructuring-bind (idx1 idx2)
            (get-height-or-repeat parsed :window (make-sliding-window 30))
          (calc-height-from-repeat idx1 idx2 parsed)))))
