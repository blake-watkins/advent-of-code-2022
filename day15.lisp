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
  "Given SENSOR, BEACON as positions, and a ROW number, return interval where there cannot be another beacon, or NIL."
  (destructuring-bind (x y) sensor
    (let* ((covered-range (manhattan sensor beacon))
           (row-range (- covered-range (abs (- y row)))))
      (when (>= row-range 0)
        (list (- x row-range) (+ x row-range))))))

(defun covered-intervals (parsed row)
  "Return list of all intervals covered by the sensors and beacons in PARSED in the given ROW."
  (intervals-normalize
   (iter
     (for (sensor beacon) in parsed)
     (for interval = (covered-interval sensor beacon row))
     (when interval (collect interval)))))

(defun find-uncovered (x intervals)
  "Find the first position at or above X that is not covered by INTERVALS."
  (let ((containing (intervals-contain intervals x)))
    (if (null containing)
        x
        (find-uncovered (1+ (interval-end containing)) intervals))))

(defun num-covered-beacons (parsed intervals row)
  "Find the number of beacons in parsed that are covered by INTERVALS in ROW. "
  (length (remove-duplicates
	   (iter
	     (for (nil (x y)) in parsed)
	     (when (and (= y row) (intervals-contain intervals x))
	       (collect x))))))

(defun day15 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (row 2000000)
	 (search-size 4000000))
    (if (= part 1)
	(let* ((intervals (covered-intervals parsed row))
	       (num-covered-beacons (num-covered-beacons parsed intervals row)))
	  (- (intervals-size intervals) num-covered-beacons))
	(destructuring-bind (x y)
	    (iter
	      (for row below search-size)
	      (for uncovered-pos =
		   (find-uncovered 0 (covered-intervals parsed row)))
	      (finding (list uncovered-pos row)
		       such-that (<= uncovered-pos search-size)))
	  (+ (* search-size x) y)))))
