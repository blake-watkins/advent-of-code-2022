(in-package :aoc-2022)

(defun parse-monkey ()
  (parse-keyword #'alphanumericp))

(defun parse-expr ()
  (either (parse-number)
          (with-monad
            (assign op1 (parse-monkey))
            (parse-space)
            (assign op (parse-keyword))
            (parse-space)
            (assign op2 (parse-monkey))
            (unit (list op op1 op2)))))

(defun parse-file ()
  (with-monad
    (assign parsed
            (parse-lines (with-monad
                           (assign name (parse-monkey))
                           (parse-string ": ")
                           (assign expr (parse-expr))
                           (unit (list name expr)))))
    (unit (iter
            (with ret = (make-hash-table :test 'eq))
            (for (name expr) in parsed)
            (setf (gethash name ret) expr)
            (finally (return ret))))))

(defstruct expr
  (x1 0) (x0 0))

(defun expr+ (e1 e2)
  (make-expr :x1 (+ (expr-x1 e1) (expr-x1 e2)) :x0 (+ (expr-x0 e1) (expr-x0 e2))))

(defun expr- (e1 e2)
  (make-expr :x1 (- (expr-x1 e1) (expr-x1 e2)) :x0 (- (expr-x0 e1) (expr-x0 e2))))

(defun expr* (e1 e2)
  (when (and (/= 0 (expr-x1 e1)) (/= 0 (expr-x1 e2)))
    (error 'cant-multiply))
  (make-expr :x1 (+ (* (expr-x1 e1) (expr-x0 e2)) (* (expr-x0 e1) (expr-x1 e2)))
             :x0 (* (expr-x0 e1) (expr-x0 e2))))

(defun expr/ (e1 e2)
  (when (or (/= 0 (expr-x1 e2)) (= 0 (expr-x0 e2)))
    (error 'cant-divide))
  (make-expr :x1 (/ (expr-x1 e1) (expr-x0 e2))             
             :x0 (/ (expr-x0 e1) (expr-x0 e2))))

(defun expr= (e1 e2)
  (make-expr :x0 (/ (- (expr-x0 e2) (expr-x0 e1)) (- (expr-x1 e1) (expr-x1 e2)))))

(defun eval-monkey (monkey monkeys part)
  (if (and (= part 2) (eq monkey :humn))
      (make-expr :x1 1 :x0 0)
      (let ((expr (gethash monkey monkeys)))
        (if (numberp expr)
            (make-expr :x0 expr)
            (apply (if (and (eq part 2) (eq monkey :root))
                       #'expr=
                       (ecase (car expr)
                         (:+ #'expr+)
                         (:* #'expr*)
                         (:/ #'expr/)
                         (:- #'expr-)))
                   (mapcar (lambda (m)
                             (eval-monkey m monkeys part))
                           (cdr expr)))))))

(defun day21 (input &key (part 1))
  (let ((monkeys (run-parser (parse-file) input)))
    (expr-x0 (eval-monkey :root monkeys part))))
