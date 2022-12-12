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
    (for crt = (mod (1- cycle) 40))
    (until (and (null instructions) (= 0 duration)))
    (when (= 20 (mod cycle 40))
      (summing (* cycle x) into part-1))
    (collect (if (<= (abs (- crt x)) 1) #\# #\.) into part-2)
    (when (= 0 (decf duration))
      (when (eq :addx (first instruction))
        (incf x (third instruction)))
      (when instructions
        (setf instruction (pop instructions))
        (setf duration (second instruction))))
    (finally (return (list part-1 part-2)))))

(defun day10 (input &key (part 1))
  (let* ((instructions (run-parser (parse-file) input))
         (processed (process instructions)))
    (if (= part 1)
        (first processed)
        (iter
          (for r from 0 below 240 by 40)
          (format t "~{~a~}~%" (subseq (second processed) r (+ r 40)))))))
