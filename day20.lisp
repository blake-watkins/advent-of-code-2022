(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (parse-number)))

(defun mix-num (nums num orig-idx)
  (let* ((idx (fset:position-if (lambda (pair) (= orig-idx (second pair)))
                                nums))
         (new-idx (mod (+ num idx) (1- (fset:size nums))))
         (without (fset:less nums idx)))
    (fset:insert without new-idx (list num orig-idx))))

(defun mix (nums)
  (iter
    (with ret = nums)
    (format t "~a~%" (fset:image #'first ret))
    (for (num idx) in-fset (fset:sort nums #'< :key #'second))
    (setf ret (mix-num ret num idx))
    (finally (return ret))))

(defun grove (nums)
  (iter
    (with zero-pos = (fset:position-if (lambda (pair) (= 0 (first pair))) nums))
    (for idx in '(1000 2000 3000))
    (sum (first (fset:lookup nums (mod (+ zero-pos idx) (fset:size nums)))))))

(defun day20 (input &key (part 1))
  (let* ((nums (mapcar (lambda (num) (* num (if (= part 1) 1 811589153)))
                       (run-parser (parse-file) input)))
         (pairs (fset:convert 'fset:seq
                              (iter
                                (for idx from 0)
                                (for num in nums)
                                (collect (list num idx))))))
    
    (iter
      (repeat (if (= part 1) 1 10))
      (setf pairs (mix pairs)))
    
    (grove pairs)))
