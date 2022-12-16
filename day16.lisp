(in-package :aoc-2022)

(defstruct valve
  name flow next-valves)

(defun parse-valve ()
  (with-monad
    (parse-string "Valve ")
    (assign name (parse-keyword))
    (assign flow-rate (parse-until (parse-number)))
    (parse-string "; tunnel")    
    (zero-or-more (parse-character #\s))
    (parse-string " lead")
    (zero-or-more (parse-character #\s))
    (parse-string " to valve")
    (zero-or-more (parse-character #\s))
    (parse-character #\Space)
    (assign tunnels (parse-list (parse-keyword) ", "))
    (unit (make-valve :name name
                      :flow flow-rate
                      :next-valves tunnels))))

(defun solve (time-remaining pressure-release position open valves visited)
  (setf (gethash (list position open) visited) t)
  (if (zerop time-remaining)
      (progn
        (remhash (list position open) visited)
        (list pressure-release (list (list position pressure-release))))
      (let ((cur-valve (gethash position valves))
            (best-amount 0)
            (best-path nil))
        (when (and (not (member position open))
                   (> (valve-flow cur-valve) 0))
          (destructuring-bind (rec-best rec-path)
              (solve (1- time-remaining)
                     (+ pressure-release
                        (* (1- time-remaining)
                           (valve-flow cur-valve)))
                     position
                     (cons position open)
                     valves
                     visited)
            (setf best-amount rec-best)
            (setf best-path (cons (list position pressure-release) rec-path))))
        (iter
          (for next-valve in (valve-next-valves cur-valve))
          (unless (gethash (list next-valve open) visited)
            (destructuring-bind (rec-best rec-path)
                (solve
                 (1- time-remaining)
                 pressure-release
                 next-valve
                 open
                 valves
                 visited)
              (if (> rec-best best-amount)
                  (setf best-amount rec-best)
                  (setf best-path (cons (list position pressure-release)
                                        rec-path))))))
        (remhash (list position open) visited)
        (list best-amount best-path))))

(defun shortest-from (pos valves)
  (let ((distances (make-hash-table :test 'eq)))
    (iter
      (for (cur-vertex parent distance)
           in-bfs-from pos
           neighbours (lambda (v)
                        (valve-next-valves (gethash v valves)))
           test 'eq
           single t)
      (setf (gethash cur-vertex distances) distance))
    distances))

(defun day16 (input)
  (let* ((parsed (run-parser (parse-lines (parse-valve)) input))
         (valves (iter
                   (with ret = (make-hash-table))
                   (for valve in parsed)
                   (setf (gethash (valve-name valve) ret) valve)
                   (finally (return ret)))))
                                        ;    (solve 30 0 :AA '() valves (make-hash-table :test 'equal))
    (shortest-from :aa valves)
    ))

(defun memoize (fn)
  (let ((memo (make-hash-table :test 'equal)))
    (lambda (time pressure pos open valves visited)
      (multiple-value-bind (ret present) (gethash (list pos open) memo)
        (if present
            ret
            (progn
              (let ((store (funcall fn time pressure pos open valves visited)))
                (setf (gethash (list pos open) memo) store)
                store)))))))
(defparameter solve-memo (memoize #'solve))

