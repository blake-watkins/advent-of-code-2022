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

(defun open-valves (time-remaining pressure valve to-open valves)
  (if (or (<= time-remaining 0)
          (fset:empty? to-open))
      pressure
      (iter
        (for (neighbour distance) in (valve-neighbours (gethash valve valves)))
        (for neighbour-open-time = (- time-remaining (+ distance 1)))
        (when (and (fset:contains? to-open neighbour))
          (maximizing
           (open-valves-2 neighbour-open-time
                          (+ pressure (* neighbour-open-time
                                         (valve-flow (gethash neighbour valves))))
                          neighbour
                          (fset:less to-open neighbour)
                          valves))))))

(defun day16 (input)
  (destructuring-bind (valves to-open) (run-parser (parse-file) input)
    (open-valves-2 30 0 :aa to-open valves)))
