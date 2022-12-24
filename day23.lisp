(in-package :aoc-2022)

(defun parse-file ()
  (parse-lines (one-or-more (either (then (parse-character "#") (unit :elf))
                                    (then (parse-character ".") (unit :blank))
                                    (then (parse-character " ") (unit :blank))))))

(defparameter *propose-order* '(:n :s :w :e))
(setf *print-circle* t)
(setf (cddddr *propose-order*) *propose-order*)

(defparameter *directions*
  '((:n (-1 0)) (:ne (-1 1)) (:e (0 1)) (:se (1 1))
    (:s (1 0)) (:sw (1 -1)) (:w (0 -1)) (:nw (-1 -1))))

(defun is-accompanied (elf elves)
  (some (lambda (direction)
          (gethash (point+ elf (second direction)) elves))
        *directions*))

(defun valid-direction (elf dir elves)
  (iter
    (with idx = (position dir *directions* :key #'first))
    (for offset from -1 to 1)
    (for direction = (elt *directions* (mod (+ idx offset) (length *directions*))))
    (for check-square = (point+ elf (second direction)))
    (never (gethash check-square elves))
    (finally (return (point+ elf (second (elt *directions* idx)))))))

(defun propose (elf order elves)
  (when (is-accompanied elf elves)
    (iter
      (repeat 4)
      (for direction in order)
      (thereis (valid-direction elf direction elves)))))

(defun get-proposals (order elves)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for (elf nil) in-hashtable elves)
    (for proposal = (propose elf order elves))
    (if proposal
        (push elf (gethash proposal ret))
        (push elf (gethash :unmoved ret)))
    (finally (return ret))))

(defun move-elves (proposals)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for (target elves) in-hashtable proposals)
    (if (and (not (eq :unmoved target))
             (= 1 (length elves)))
        (setf (gethash target ret) t)
        (iter
          (for elf in elves)
          (setf (gethash elf ret) t)))
    (finally (return ret))))

(defun do-round (elves)
  (let* ((proposals (get-proposals *propose-order* elves))
         (moved (move-elves proposals)))
    (pop *propose-order*)
    (values moved (= (length (gethash :unmoved proposals))
                     (hash-table-count elves)))))

(defun bounds (elves)
  (iter
    (for ((r c) nil) in-hashtable elves)
    (minimizing r into min-r)
    (minimizing c into min-c)
    (maximizing r into max-r)
    (maximizing c into max-c)
    (finally (return (list min-r min-c max-r max-c)))))

(defun calc-empty (elves bounds)
  (destructuring-bind (min-r min-c max-r max-c) bounds
    (- (* (1+ (- max-r min-r))
          (1+ (- max-c min-c)))
       (hash-table-count elves))))

(defun day23 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (elves (iter
                  (with ret = (make-hash-table :test 'equal))
                  (for r from 0)
                  (for row in parsed)
                  (iter
                    (for c from 0)
                    (for square in row)
                    (when (eq :elf square)
                      (setf (gethash (list r c) ret) t)))
                  (finally (return ret)))))

    (setf *propose-order* '(:n :s :w :e))
    (setf (cddddr *propose-order*) *propose-order*)
    
    (iter
      (for round from 1)
      (multiple-value-bind (new-elves done)
          (do-round elves)
        (until done)
        (setf elves new-elves))
      (finally (return round)))))
