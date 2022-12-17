(in-package :aoc-2022)

(defstruct valve
  name flow next-valves neighbours)

(defun parse-plural (item)
  (with-monad
    (parse-string item)
    (zero-or-more (parse-character #\s))))

(defun parse-valve ()
  (with-monad
    (parse-string "Valve ")
    (assign name (parse-keyword))
    (assign flow-rate (parse-until (parse-number)))
    (parse-plural "; tunnel")    
    (parse-plural " lead")
    (parse-plural " to valve")
    (parse-character #\Space)
    (assign tunnels (parse-list (parse-keyword #'alphanumericp) ", "))
    (unit (make-valve :name name
                      :flow flow-rate
                      :next-valves tunnels
                      :neighbours nil))))

(defun parse-file ()
  (with-monad
    (assign valves (parse-lines (parse-valve)))
    (unit (iter
            (with ret = (make-hash-table))
            (with to-open = (fset:empty-set))
            (for valve in valves)
            (setf (gethash (valve-name valve) ret) valve)
            (when (> (valve-flow valve) 0)
              (fset:includef to-open (valve-name valve)))
            (finally
             (set-neighbours ret to-open)
             (return (list ret to-open)))))))

(defun set-neighbours (valves to-open)
  (iter
    (for (name valve) in-hashtable valves)
    (setf (valve-neighbours valve) '())
    (iter
      (for (neighbour parent distance)
           in-bfs-from name
           neighbours (lambda (v)
                        (valve-next-valves (gethash v valves)))
           test 'eq
           single t)
      (when (and (not (eq name neighbour))
                 (fset:contains? to-open neighbour))
        (push (list neighbour distance) (valve-neighbours valve))))
    (setf (valve-neighbours valve)
          (sort (valve-neighbours valve) #'>
                :key (lambda (neighbour) (valve-flow (gethash (first neighbour) valves)))))))

(defparameter *cache* (fset:empty-map 0))

(defun open-valves (time-remaining valve to-open valves)
  (let ((ret 0))
    (unless (or (<= time-remaining 0)
                (fset:empty? to-open))
      (if (fset:domain-contains? *cache* (list time-remaining valve to-open))
          (setf ret
                (fset:lookup *cache* (list time-remaining valve to-open)))          
          (iter
            (for (neighbour distance) in (valve-neighbours (gethash valve valves)))
            (for neighbour-open-time = (- time-remaining (+ distance 1)))
            (when (and (fset:contains? to-open neighbour))
              (maximizing (+ (* neighbour-open-time
                                (valve-flow (gethash neighbour valves)))
                             (open-valves neighbour-open-time
                                          neighbour
                                          (fset:less to-open neighbour)
                                          valves))
                          into rec))
            (finally (setf ret rec)))))
    (setf *cache*
          (fset:with *cache*
                     (list time-remaining valve to-open)
                     (max (fset:lookup *cache* (list time-remaining valve to-open))
                          ret)))
    ret))

(defun day16 (input &key (part 1))
  (destructuring-bind (valves to-open) (run-parser (parse-file) input)
    (if (= part 1)
        (open-valves 30 :aa to-open valves)
        (iter
          (for count from 1)
          (when (zerop (mod count 100))
            (format t "~a ~a~%" count max-score))
          (for my-valves-list in (combinations (fset:convert 'list to-open)))
          (for my-valves = (fset:convert 'fset:set my-valves-list))
          (for elephant-valves = (fset:set-difference to-open my-valves))
          (for my-score = (open-valves 26 :aa my-valves valves))
          (for elephant-score = (open-valves 26 :aa elephant-valves valves))
          (maximizing (+ my-score elephant-score) into max-score)))))
