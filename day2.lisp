(in-package :aoc-2022)

(defun parse-game ()
  (parse-list (parse-keyword) (parse-space)))

(defun parse-file ()
  (parse-lines (parse-game)))

;; convert the parsed list into appropriate symbols
(defun convert (parsed)
  (mapcar (lambda (c)
            (ecase c ((:A :X) :rock) ((:B :Y) :paper) ((:C :Z) :sizzors)))
          parsed))

(defparameter *beats*
  '((:rock . :sizzors) (:sizzors . :paper) (:paper . :rock)))

(defun winning-move-against (move) (car (rassoc move *beats*)))
(defun losing-move-against (move) (cdr (assoc move *beats*)))

;; Given opponent and my move as a list, return whether I win, lose, draw.
(defun outcome (moves)
  (destructuring-bind (opponent-move my-move) moves
    (cond
      ((eq opponent-move my-move) :draw)
      ((eq my-move (winning-move-against opponent-move)) :win)
      (t :lose))))

;; Return game score based on my move and the outcome
(defun score (my-move outcome)
  (+ (ecase my-move (:rock 1) (:paper 2) (:sizzors 3))
     (* 3 (ecase outcome (:lose 0) (:draw 1) (:win 2)))))

;; Convert the parsed list into symbols for part 2
(defun convert-2 (parsed)
  (mapcar (lambda (c)
            (ecase c
              (:A :rock) (:B :paper) (:C :sizzors)
              (:X :lose) (:Y :draw) (:Z :win)))
          parsed))

;; Given opponent's move and desired outcome, return my move.
(defun move-for-outcome (move-outcome)
  (destructuring-bind (opponent-move outcome) move-outcome
    (ecase outcome
      (:draw opponent-move)
      (:win (winning-move-against opponent-move))
      (:lose (losing-move-against opponent-move)))))

(defun day2 (input &key (part 1))
  (labels ((score-1 (parsed)
             (let ((moves (convert parsed)))
               (score (second moves) (outcome moves))))
           (score-2 (parsed)
             (let ((move-outcome (convert-2 parsed)))
               (score (move-for-outcome move-outcome)
                      (second move-outcome)))))
    (let ((parsed (run-parser (parse-file) input)))
      (reduce #'+ (mapcar (if (eq part 1) #'score-1 #'score-2) parsed)))))

