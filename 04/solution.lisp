(defpackage :aoc2021-day04
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day04)

;; Read input
(defun read-input (filename)
  (with-open-file (in filename)

    (flet ((line-to-list (line)
             (read-from-string (concatenate 'string "(" line ")"))))

      (let ((numbers (mapcar #'parse-integer (split-sequence:split-sequence #\, (read-line in))))
            (boards (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
                          (ret (list line) (cons line ret)))
                         ((eq line 'eof) (reverse (cdr ret))))))

        (values numbers
                (split-sequence:split-sequence 'nil (mapcar #'line-to-list boards)
                                               :remove-empty-subseqs t))))))

;; Helpers
(defun cross-out-number (num boards)
  (subst -1 num boards))

(defun bingo-p (board)
  (or (find -5 (mapcar (lambda (row) (apply #'+ row)) board))
      (find -5 (reduce (lambda (acc row) (mapcar #'+ acc row)) board))))

(defun play-bingo (nums boards &optional last-num)
  (let ((winning-board (find-if #'bingo-p boards)))
    (if winning-board
        (values winning-board last-num nums boards)
        (play-bingo (cdr nums) (cross-out-number (car nums) boards) (car nums)))))

;; Part 1 solution
(defun solve-1 (filename)
  (multiple-value-bind (nums boards) (read-input filename)
    (multiple-value-bind (winning-board last-num) (play-bingo nums boards)
      (* last-num (reduce #'+ (reduce #'append (subst 0 -1 winning-board)))))))

;; Part 2 solution
(defun solve-2 (filename)
  (multiple-value-bind (nums boards) (read-input filename)
    (labels ((play-again (nums boards)
               (multiple-value-bind (winning-board last-num nums boards) (play-bingo nums boards)
                 (if (= 1 (length boards))
                     (* last-num (reduce #'+ (reduce #'append (subst 0 -1 winning-board))))
                     (play-again nums (remove winning-board boards :test #'equal))))))
      (play-again nums boards))))
