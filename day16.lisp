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
            (with to-open = '())
            (for valve in valves)
            (setf (gethash (valve-name valve) ret) valve)
            (when (> (valve-flow valve) 0)
              (push (valve-name valve) to-open))
            (finally
             (set-neighbours ret to-open)
             (return (list ret
                           (make-array (length to-open)
                                       :element-type 'bit
                                       :initial-element 1)
                           to-open)))))))

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
                 (member neighbour to-open))
        (push (list neighbour distance) (valve-neighbours valve))))
    (setf (valve-neighbours valve)
          (sort (valve-neighbours valve) #'>
                :key (lambda (neighbour) (valve-flow (gethash (first neighbour) valves)))))))

(defparameter *cache* nil)

(defun remove-valve (valve-name to-open valve-names)
  (let ((ret (copy-seq to-open)))
    (setf (bit ret (position valve-name valve-names)) 0)
    ret))

(defun valves-open (to-open)
  (every (lambda (x) (= 0 x)) to-open))

(defun valve-closed (valve to-open valve-names)
  (= 1 (bit to-open (position valve valve-names))))

(defun list-to-to-open (valves valve-names)
  (let ((ret (make-array (length valve-names)
                         :element-type 'bit
                         :initial-element 0)))
    (iter
      (for valve in valves)
      (setf (bit ret (position valve valve-names)) 1)
      (finally (return ret)))))

(defun open-valves (key valves valve-names)
  (destructuring-bind (time-remaining valve to-open) key
    (let ((ret 0))
      (unless (or (<= time-remaining 0) (valves-open to-open))
        (if (gethash key *cache*)
            (setf ret (gethash key *cache*))
            (iter
              (for (neighbour distance) in
                   (valve-neighbours (gethash valve valves)))
              (for neighbour-open-time = (- time-remaining (+ distance 1)))
              (when (valve-closed neighbour to-open valve-names)
                (maximizing (+ (* neighbour-open-time
                                  (valve-flow (gethash neighbour valves)))
                               (open-valves (list  neighbour-open-time
                                                   neighbour
                                                   (remove-valve neighbour
                                                                 to-open
                                                                 valve-names))
                                            valves
                                            valve-names))
                            into rec))
              (finally (setf ret rec)))))
      (setf (gethash key *cache*) (max (gethash key *cache* 0) ret))
      ret)))

(defun day16 (input &key (part 1))
;  (setf *cache* (make-hash-table :test 'equal))
  (destructuring-bind (valves to-open valve-names) (run-parser (parse-file) input)
    (if (= part 1)
        (open-valves (list  30 :aa to-open) valves valve-names)
        (iter
          (for count from 1)
          (when (zerop (mod count 100))
            (format t "~a ~a~%" count max-score))
          (for my-valves-list in (combinations valve-names))
          (for my-valves = (list-to-to-open my-valves-list valve-names))
          (for elephant-valves = (bit-not my-valves))
          (for my-score = (open-valves (list 26 :aa my-valves) valves valve-names))
          (for elephant-score = (open-valves (list 26 :aa elephant-valves) valves valve-names))
          (maximizing (+ my-score elephant-score) into max-score)
          (finally (return max-score))))))
