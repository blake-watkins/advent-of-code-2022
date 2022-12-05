(in-package :aoc-2022)

(defun parse-crate ()
  (with-monad
    (parse-character #\[)
    (assign crate (parse-alphanumeric))
    (parse-character #\])
    (unit (intern (string  crate) :keyword))))

(defun parse-blank ()
  (with-monad
    (n-of 3 (parse-character #\Space))
    (unit :blank)))

(defun parse-crates ()
  (parse-lines (parse-list (either (parse-blank) (parse-crate)) " ")))

(defun parse-move ()
  (with-monad
    (parse-string "move ")
    (assign amount (parse-number))
    (parse-string " from ")
    (assign from (parse-number))
    (parse-string " to ")
    (assign to (parse-number))
    (unit (list amount from to))))

(defun parse-file ()
  (with-monad
    (assign crates (parse-crates))
    (n-of 3 (parse-until (parse-newline)))
    (assign moves (parse-lines (parse-move)))
    (unit (list crates moves))))

(defun get-stacks (crates)
  (iter
    (with ret = (iter (repeat (length (first crates))) (collect '())))
    (for level in (reverse crates))
    (iter
      (for crate in level)
      (for i from 0)
      (unless (eq :blank crate)
        (push crate (elt ret i))))
    (finally (return ret))))

(defun move (amount from to stacks &key (reverse nil))
  (let ((crates (subseq (elt stacks from) 0 amount)))
    (setf (elt stacks from) (subseq (elt stacks from) amount))
    (setf (elt stacks to)
          (concatenate 'list
                       (if reverse (reverse crates) crates)
                       (elt stacks to))))
  stacks)

(defun day5 (input &key (part 1))
  (destructuring-bind (crates moves) (run-parser (parse-file) input)
    (let ((stacks (get-stacks crates)))
      (iter
        (for (amount from to) in moves)
        (setf stacks (move amount (1- from) (1- to) stacks :reverse (= part 1))))
      stacks)))
