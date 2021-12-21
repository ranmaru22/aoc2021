(defpackage :aoc2021-day21
  (:use #:cl #:alexandria)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day21)

(defun mod* (n m)
  "Returns `mod' of N and M but skips 0."
  (1+ (mod (1- n) m)))

(defun roll-die (last-roll)
  "Returns a list of rolls on the deterministic die."
  (list (mod* (+ 1 last-roll) 100)
        (mod* (+ 2 last-roll) 100)
        (mod* (+ 3 last-roll) 100)))

(defun move-pawn (start-pos rolls score)
  "Returns the new position and score after moving a pawn at START-POS with
ROLLS."
  (let* ((roll-sum (apply #'+ rolls))
         (end-pos (mod* (+ start-pos roll-sum) 10)))
    (list end-pos (+ score end-pos) (caddr rolls))))

(defun turn (player round last-roll)
  "Simulate one turn of the game for PLAYER."
  (destructuring-bind (new-pos new-score new-last-roll)
      (move-pawn (car player) (roll-die last-roll) (cadr player))
    (list (list new-pos new-score) (1+ round) new-last-roll)))

(defun game (player opponent win-score &optional (round 0) (last-roll 0))
  "Simulate the game between PLAYER and OPPONENT."
  (destructuring-bind (new-player new-round new-last-roll)
      (turn player round last-roll)
    (if (>= (cadr new-player) win-score)
        (* (cadr opponent) (* 3 new-round))
        (game opponent new-player win-score new-round new-last-roll))))

;; Part 1 solution
(defun solve-1 ()
  (game '(7 0) '(10 0) 1000))

(defun move-pawn* (start-pos roll score)
  "Like `move-pawn', but only takes a single roll."
  (let ((end-pos (mod* (+ start-pos roll) 10)))
    (list end-pos (+ score end-pos))))

(defun game* (player opponent &optional (win-score 21))
  "Many-worlds quantum version of `game'. Returns the number of world lines in
which the winning player wins given PLAYER and OPPONENT as starting positions
and WIN-SCORE as the target score."
  (let ((cache (make-hash-table :test #'equal))
        ;; List of all possible dice outcomes and how often they come up.
        (rolls '((3 1) (4 3) (5 6) (6 7) (7 6) (8 3) (9 1))))

    (symbol-macrolet
        ((this (gethash `(,pl ,op ,pscore ,oscore) cache)))

      (labels ((rec (pl op pscore oscore)
                 (cond
                   ;; If the result is already in the cache, return it.
                   (this this)

                   ;; If it's the end of the game, return 1 for the winner and
                   ;; 0 for the opponent.
                   ((>= pscore win-score) '(1 0))
                   ((>= oscore win-score) '(0 1))

                   ;; Otherwise loop through all possible branches from here
                   ;; and sum them up, and add the final result to the cache.
                   (t (loop :for (roll x) :in rolls
                            :for (np ns) := (move-pawn* pl roll pscore)
                            :for (owin pwin) := (rec op np oscore ns)
                            :sum (* x pwin) :into pwins
                            :sum (* x owin) :into owins
                            :finally (return (setf this (list pwins owins))))))))

        ;; We start with scores of 0 for botho
        (rec player opponent 0 0)))))

;; Part 2 solution
(defun solve-2 ()
  (apply #'max (game* 7 10 21)))
