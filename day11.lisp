(in-package :aoc-2022)

(defun parse-operation ()
  (parse-list (either (parse-number) (parse-keyword)) " "))

(defun parse-monkey ()
  (with-monad
    (parse-string "Monkey ")
    (assign id (parse-number))
    (parse-until (parse-string "Starting items: "))
    (assign items (parse-list (parse-number) ", "))
    (parse-until (parse-string "Operation: new = "))
    (assign operation (parse-operation))
    (parse-until (parse-string "Test: divisible by "))
    (assign test-num (parse-number))
    (assign throw-to (n-of 2 (with-monad
                               (parse-until (parse-string "throw to monkey "))
                               (parse-number))))
    (n-of 2 (parse-newline))
    (unit (list id items operation test-num throw-to))))

(defun apply-operation (old operation)
  (let ((substituted (substitute old :old operation)))
    (funcall (ecase (second substituted)
               (:+ #'+)
               (:* #'*))
             (first substituted)
             (third substituted))))

(defun monkey-turn (items operation test throw-to modulus)
  (iter
    (with thrown-items = (make-hash-table))
    (for item in items)
    (for worry-level = (mod (apply-operation item operation) modulus))
    (push worry-level (gethash (if (= 0 (mod worry-level test))
                                   (first throw-to)
                                   (second throw-to))
                               thrown-items
                               '()))
    (finally (return thrown-items))))

(defun parse-file ()
  (one-or-more (parse-monkey)))

(defun day11 (input)
  (let* ((monkeys (run-parser (parse-file) input))         
         (monkey-items (iter
                         (with ret = (make-hash-table))
                         (for monkey in monkeys)
                         (setf (gethash (first monkey) ret)
                               (second monkey))
                         (finally (return ret))))
         (monkey-inspections (make-hash-table))
         (modulus (reduce #'* (mapcar #'fourth monkeys))))
    (iter
      (repeat 10000)
      (iter
        (for (id _ operation test throw-to) in monkeys)
        (for items = (gethash id monkey-items))

        (incf (gethash id monkey-inspections 0) (length items))

        (for thrown-items = (monkey-turn items
                                         operation
                                         test
                                         throw-to
                                         modulus))
        (iter
          (for (to-id items) in-hashtable thrown-items)
          (if (null (gethash to-id monkey-items))
              (setf (gethash to-id monkey-items) (reverse items))
              (nconc (gethash to-id monkey-items)
                     (reverse items))))
        (setf (gethash id monkey-items) '()))
      (finally (return monkey-inspections)))))
