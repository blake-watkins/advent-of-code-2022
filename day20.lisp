(in-package :aoc-2022)

(defun parse-file ()
  (with-monad
    (assign ret (parse-lines (parse-number)))
    (unit (fset:convert 'fset:seq ret))))

(defun mix-num (nums num orig-idx)
  (let* ((idx (fset:position-if (lambda (pair)
                                  (= orig-idx (second pair)))
                                nums))
         (new-idx (mod (+ num (if (= idx (1- (fset:size nums)))
                                  0
                                  idx))
                       (1- (fset:size nums))))
         (without (fset:less nums idx)))
    (fset:insert without new-idx (list num orig-idx))))

(defun mix (nums)
  (iter
    (with ret = nums)
    (for (num idx) in-fset (fset:sort  nums #'< :key #'second))
    (setf ret (mix-num ret num idx))
;;    (format t "~a~%" ret)
    (finally (return ret))))

(defun grove (mixed)
  (iter
    (with pos = (fset:position-if (lambda (pair)
                                    (= 0 (first pair)))
                                  mixed))
    (for idx in '(1000 2000 3000))
    (sum (first (fset:lookup mixed (mod (+ idx pos) (fset:size mixed)))))))

(defun day20 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (indexed
           (iter
             (with ret = parsed)
             (for idx from 0)
             (for num in-fset parsed)
             (setf ret (fset:with ret idx (list num idx)))
             (finally (return ret)))))
    (grove (mix indexed))))

(defun day20-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (indexed
           (iter
             (with ret = parsed)
             (for idx from 0)
             (for num in-fset parsed)
             (setf ret (fset:with ret idx (list (* num 811589153) idx)))
             (finally (return ret)))))
    (iter
      (repeat 10
              )
      (setf indexed (mix indexed)))
    
    (grove indexed)))
