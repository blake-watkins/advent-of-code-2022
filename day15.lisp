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

(defun sensor-range (sensor beacon)
  (manhattan sensor beacon))

(defun covered-interval (sensor beacon row)
  (destructuring-bind (x y) sensor
    (let ((covered-range (sensor-range sensor beacon))
          (diff (abs (- y row))))
      (if (<= diff covered-range)
          (list (+ (- x covered-range) diff)
                (- (+ x covered-range) diff))
          nil))))

(defun interval-contains (interval value)
  (<= (first interval) value (second interval)))

(defun intervals-contain (intervals value)
  (iter
    (for interval in intervals)
    (finding interval such-that (interval-contains interval value))))


(defun not-present-intervals (parsed row)
  (iter
      (for (sensor beacon) in parsed)
      (for interval = (covered-interval sensor beacon row))
      (when interval
        (collect interval))))

(defun intervals-size (dimensions intervals)
  (format t "Range ~a~%" dimensions)
  (destructuring-bind (start end) dimensions
    (let ((containing-interval (intervals-contain intervals start)))
      (if containing-interval
          (let ((int-end (second containing-interval)))
            (let ((new-end (min end int-end)))
              (format t "Found containing interval ~a, new end ~a~%" containing-interval new-end)
              (if (= end new-end)
                  (progn
                    (format t "Returning ~a~%" (1+ (- new-end start)))
                    (1+ (- new-end start)))
                  (progn
                    (format t "Adding ~a then calculating from ~a~%"
                            (1+ (- new-end start)) (list (1+ new-end) end))
                    (+ (1+ (- new-end start))
                       (intervals-size (list (1+ new-end) end) intervals))))))
          (let ((new-int-start
                  (iter
                    (for (x nil) in intervals)
                    (when (> x start)
                      (minimizing x)))))
            (if (and new-int-start (<= new-int-start end))
                (progn
                  (format t "Next interval ~a~%" (list new-int-start end))
                  (intervals-size (list new-int-start end) intervals))
                (progn
                  (format t "No next interval~%")
                  0)))))))

(defun merge-intervals (i1 i2)
  (destructuring-bind (s1 e1) i1
    (destructuring-bind (s2 e2) i2
      (cond
        ((<= s1 s2 e2 e1) (list i1))
        ((<= s2 s1 e1 e2) (list i2))
        ((<= s1 s2 e1) (list (list s1 (max e1 e2))))
        ((<= s2 s1 e2) (list (list s2 (max e1 e2))))
        (t (list i1 i2))))))

(defun day15 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (row 2000000)
         (intervals (not-present-intervals parsed row))
         (dim-x (iter
                  (for sb in parsed)
                  (for (xs nil) = (first sb))
                  (for (xb nil) = (second sb))                  
                  (minimizing (min xs xb) into min-x)
                  (maximizing (max xs xb) into max-x)
                  (finally (return (list (- min-x 100000000000) (+ max-x 100000000000))))))
         (num-included-beacons
           (length (remove-duplicates  (iter
                                         (for sb in parsed)
                                         (for (xb yb) = (second sb))
                                         (when (and (= yb row)
                                                    (intervals-contain intervals xb))
                                           (collect (second sb))))
                                       :test 'equal))))
    (format t "~a ~a ~a ~a~%" dim-x intervals (intervals-size dim-x intervals)
            num-included-beacons)
    (- (intervals-size dim-x intervals) num-included-beacons)))
