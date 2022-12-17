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

(defun upper-bound-pressure (time-remaining valve to-open valves)
  (fset:reduce #'+
               (fset:image
                (lambda (v)
                  (* (valve-flow (gethash v valves))
                     (max 0 (- time-remaining
                               (let ((n-dist
                                       (find-if (lambda (i) (eq (first i) v))
                                                (valve-neighbours
                                                 (gethash valve valves)))))
                                 (if n-dist
                                     (second n-dist)
                                     0))))))
                to-open)))

(defun open-valves (time-remaining pressure valve to-open valves visited)
;  (break)
  (if (or (<= time-remaining 0)
          (fset:empty? to-open))
      pressure
      (let ((best-pressure pressure))
        (when (and (fset:contains? to-open valve)
                   (> (valve-flow (gethash valve valves)) 0))
          (let ((open-pressure (open-valves (1- time-remaining)
                                            (+ pressure
                                               (* (1- time-remaining)
                                                  (valve-flow (gethash valve valves))))
                                            valve
                                            (fset:less to-open valve)
                                            valves
                                            (fset:with visited
                                                       (list valve to-open)))))
            (setf best-pressure (max best-pressure open-pressure))))
        (when (> (+ pressure (upper-bound-pressure time-remaining valve to-open valves))
                 best-pressure)
          (iter
            (for (neighbour distance) in (valve-neighbours (gethash valve valves)))
            (when (and (fset:contains? to-open neighbour)
                       (not (fset:contains? visited (list neighbour to-open))))
              (let ((goto-neighbour-pressure
                      (open-valves (- time-remaining distance)
                                   pressure
                                   neighbour
                                   to-open
                                   valves
                                   (fset:with visited
                                              (list valve to-open)))))
                (setf best-pressure (max best-pressure goto-neighbour-pressure))))))
        best-pressure)))

(defun day16 (input)
  (let* ((parsed (run-parser (parse-lines (parse-valve)) input))
         (valves (iter
                   (with ret = (make-hash-table))
                   (for valve in parsed)
                   (setf (gethash (valve-name valve) ret) valve)
                   (finally (return ret))))
         (to-open (iter
                    (with ret = (fset:empty-set))
                    (for (name valve) in-hashtable valves)
                    (when (> (valve-flow valve) 0)
                      (fset:includef ret name))
                    (finally (return ret)))))
    (set-neighbours valves to-open)
    (open-valves 30 0 :aa to-open valves (fset:empty-set))))



(defun memoize (fn)
  (let ((memo (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (multiple-value-bind (ret present) (gethash args memo)
        (if present
            ret
            (progn
              (let ((store (apply fn args)))
                (setf (gethash args memo) store)
                store)))))))
(defparameter open-valves-memo (memoize #'open-valves))

