(in-package :aoc-2022)

(defun parse-coordinate ()
  (with-monad
    (parse-string "x=")
    (assign x (parse-number))
    (parse-string ", y=")
    (assign y (parse-number))
    (unit (list x y))))

(defun parse-file ()
  (zero-or-more (n-of 2 (parse-until (parse-coordinate)))))

(defun covered-interval (sensor beacon row)
  "Given SENSOR, BEACON as positions, and a ROW number, return interval where there cannot be another beacon, or nil."
  (destructuring-bind (x y) sensor
    (let ((covered-range (manhattan sensor beacon))
          (diff (abs (- y row))))
      (if (<= diff covered-range)
          (list (+ (- x covered-range) diff)
                (- (+ x covered-range) diff))
          nil))))

(defun covered-intervals (parsed row)
  "Return list of all intervals covered by the sensors and beacons in PARSED in the given ROW."
  (iter
      (for (sensor beacon) in parsed)
      (for interval = (covered-interval sensor beacon row))
    (when interval (collect interval))))

(defun intervals-contain (intervals value)
  "Return an interval containing VALUE if one exists, otherwise NIL."
  (iter
    (for interval in intervals)
    (for (start end) = interval)
    (finding interval such-that (<= start value end))))

(defun intervals-size (dimensions intervals)
  (destructuring-bind (start end) dimensions
    (let ((containing-interval (intervals-contain intervals start)))
      (if containing-interval
          (let ((int-end (second containing-interval)))
            (let ((new-end (min end int-end)))
              (if (= end new-end)
                  (1+ (- new-end start))
                  (+ (1+ (- new-end start))
                     (intervals-size (list (1+ new-end) end) intervals)))))
          (let ((new-int-start
                  (iter
                    (for (x nil) in intervals)
                    (when (> x start)
                      (minimizing x)))))
            (if (and new-int-start (<= new-int-start end))
                (intervals-size (list new-int-start end) intervals)
                0))))))

(defun find-uncovered (x intervals)
  (let ((containing (intervals-contain intervals x)))
    (if (null containing)
        x
        (find-uncovered (1+(second containing)) intervals))))

(defun day15 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (row 2000000)
         (intervals (covered-intervals parsed row))
         (dim-x (list -100000000000 100000000000))
         (num-included-beacons
           (length (remove-duplicates  (iter
                                         (for sb in parsed)
                                         (for (xb yb) = (second sb))
                                         (when (and (= yb row)
                                                    (intervals-contain intervals xb))
                                           (collect (second sb))))
                                       :test 'equal))))
    (find-uncovered 0 (covered-intervals parsed 2860779))))
    (iter
      (for row below 4000000)
      (finding row such-that (not (= 4000001 (intervals-size '(0 4000000) (covered-intervals parsed row))))))
