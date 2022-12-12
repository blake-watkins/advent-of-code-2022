(in-package :aoc-2022)

(defstruct monkey
  id items operation test-num throw-to inspections)

(defun parse-monkey ()
  (with-monad
    (parse-string "Monkey ")
    (assign id (parse-number))
    (parse-until (parse-string "Starting items: "))
    (assign items (parse-list (parse-number) ", "))
    (parse-until (parse-string "Operation: new = "))
    (assign operation (parse-list (either (parse-number) (parse-keyword)) " "))
    (parse-until (parse-string "Test: divisible by "))
    (assign test-num (parse-number))
    (assign throw-to (n-of 2 (with-monad
                               (parse-until (parse-string "throw to monkey "))
                               (parse-number))))
    (n-of 2 (parse-newline))
    (unit (make-monkey :id id
                       :items items
                       :operation operation
                       :test-num test-num
                       :throw-to throw-to
                       :inspections 0))))

(defun parse-file ()
  (one-or-more (parse-monkey)))

(defun apply-operation (old-val operation)
  (let ((substituted (substitute old-val :old operation)))
    (funcall (ecase (second substituted) (:+ #'+) (:* #'*))
             (first substituted)
             (third substituted))))

(defun monkey-turn (monkey monkeys modulus part)
  (with-slots (items operation test-num throw-to inspections) monkey
    (iter
      (for item in items)
      (incf inspections)
      (for worry-level = (mod (floor (apply-operation item operation)
                                     (if (= part 1) 3 1))
                              modulus))
      (for target-id = (if (zerop (mod worry-level test-num))
                           (first throw-to)
                           (second throw-to)))
      (with-slots ((target-items items)) (elt monkeys target-id)
        (setf target-items (append target-items (list worry-level)))))
    (setf items '())))

(defun monkey-business (monkeys)
  (reduce #'* (subseq (sort (mapcar #'monkey-inspections monkeys) #'>) 0 2)))

(defun day11 (input &key (part 1))
  (let* ((monkeys (run-parser (parse-file) input))         
         (modulus (reduce #'* (mapcar #'monkey-test-num monkeys))))
    (iter
      (repeat (if (= part 1) 20 10000))
      (iter
        (for monkey in monkeys)
        (monkey-turn monkey monkeys modulus part))
      (finally (return (monkey-business monkeys))))))
