(in-package :aoc-2022)

(defun parse-noop ()
  (with-monad
    (parse-string "noop")
    (unit (list :noop 1))))

(defun parse-addx ()
  (with-monad
    (parse-string "addx ")
    (assign num (parse-number))
    (unit (list :addx 2 num))))

(defun parse-file ()
  (parse-lines (either (parse-addx) (parse-noop))))

(defun process (instructions)
  (iter
    (with instruction = (pop instructions))
    (with duration = (second instruction))
    (with x = 1)
    (for cycle from 1)
    (for crt from 0)
    (until (and (null instructions) (= 0 duration)))
    (format t
            "cycle: ~a instruction: ~a duration: ~a x: ~a~%"
            cycle
            (first instruction)
            duration
            x)
    (collect (if (<= (abs (- (mod crt 40) x)) 1) #\# #\.))
    (when (> duration 0)
      (decf duration))
    (when (= 0 duration)
      (ecase (first instruction)
        (:noop nil)
        (:addx (incf x (third instruction))))
      (when instructions
        (setf instruction (pop instructions))
        (setf duration (second instruction))))))

(defun day10 (input)
  (let* ((instructions (run-parser (parse-file) input))
         (chars (process instructions)))
    (iter
      (for r from 0 below 240 by 40)
      (format t "~{~a~}~%" (subseq chars r (+ r 40))))
    (length chars)
    chars))
