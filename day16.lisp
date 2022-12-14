(in-package :aoc-2022)

(defstruct valve
  name flow next-valves neighbours position)

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
            (with valve-names = '())
            (for valve in valves)
            (setf (gethash (valve-name valve) ret) valve)
            (when (> (valve-flow valve) 0)
              (push (valve-name valve) valve-names))
            (finally
             (set-neighbours ret valve-names)
             (return (list ret (list-to-opened '() valve-names) valve-names)))))))

;; Build list of all reachable open valves & their distance
;; Store position of valve in to-open list
(defun set-neighbours (valves to-open)
  (iter
    (for (name valve) in-hashtable valves)
    (setf (valve-neighbours valve) '())
    (iter
      (for (neighbour parent distance)
           in-bfs-from name
           neighbours (lambda (v)
                        (valve-next-valves (gethash v valves)))
           test 'eq single t)
      (when (and (not (eq name neighbour)) (member neighbour to-open))
        (push (list neighbour distance) (valve-neighbours valve))))
    (setf (valve-position valve) (position name to-open))))

(defparameter *best-path-for-set* (make-hash-table :test 'equal))

(defun open-valve (valve opened)
  (let ((ret (copy-seq opened)))
    (setf (bit ret (valve-position valve)) 1)
    ret))

(defun valve-closed (valve opened)
  (= 0 (bit opened (valve-position valve))))

(defun valve-open (valve opened)
  (= 1 (bit opened (valve-position valve))))

(defun list-to-opened (valves valve-names)
  (let ((ret (make-array (length valve-names)
                         :element-type 'bit
                         :initial-element 0)))
    (iter
      (for valve in valves)
      (setf (bit ret (position valve valve-names)) 1)
      (finally (return ret)))))

(defun opened-to-list (opened valve-names)
  (iter
    (for open in-sequence opened)
    (for name in valve-names)
    (when (= 1 open) (collect name))))

;; given the time remaining, current valve, current released pressure, which
;; valves are open (and valve information)
;; return the most pressure that can be added from this point
(defun pressure-for-set (time-remaining valve pressure opened valves)
  (setf (gethash opened *best-path-for-set*)
        (max (gethash opened *best-path-for-set* 0) pressure))
  (if (= time-remaining 0)
      0
      (or (iter
            (for (neighbour distance) in (valve-neighbours (gethash valve valves)))
            (for neighbour-open-time = (- time-remaining (+ distance 1)))
            (for neighbour-valve = (gethash neighbour valves))
            (when (and (valve-closed neighbour-valve opened)
                       (>= neighbour-open-time 0))
              (maximizing
               (+ (* neighbour-open-time (valve-flow neighbour-valve))
                  (pressure-for-set neighbour-open-time
                                    neighbour
                                    (+ pressure
                                       (* neighbour-open-time
                                          (valve-flow neighbour-valve)))
                                    (open-valve neighbour-valve opened)
                                    valves)))))
          0)))

(defun day16 (input &key (part 1))
  (setf *best-path-for-set* (make-hash-table :test 'equal))
  (destructuring-bind (valves opened valve-names) (run-parser (parse-file) input)
    (if (= part 1)
        (pressure-for-set 30 :aa 0 opened valves)
        (progn          
          (pressure-for-set 26 :aa 0 opened valves)
          (iter outer
            (for (my-valves my-score) in-hashtable *best-path-for-set*)
            (for unopened-valves = (bit-not my-valves))
            (for unopened-list = (opened-to-list unopened-valves valve-names))
            (iter
              (for elephant-list in (combinations unopened-list))
              (for elephant-valves = (list-to-opened elephant-list valve-names))
              (for elephant-score =
                   (gethash elephant-valves *best-path-for-set*))              
              (when (and (not (null my-score)) (not (null elephant-score)))
                (in outer (maximizing (+ my-score elephant-score))))))))))
