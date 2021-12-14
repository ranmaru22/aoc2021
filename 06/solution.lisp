(defpackage :aoc2021-day06
  (:use #:cl
        #:split-sequence)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day06)

;; Read input
(defun read-file (filename)
  (with-open-file (in filename)
    (mapcar #'parse-integer (split-sequence #\, (read-line in nil)))))

;; Helpers
(defun make-fish-list (inp)
  (let ((ret))
    (dotimes (n 9) (setf ret (acons n 0 ret)))
    (dolist (n inp) (incf (cdr (assoc n ret))))
    ret))

(defun next-day (fish)
  (let ((prev (cdr (assoc 0 fish)))
        (curr))
    (setf (cdr (assoc 7 fish)) (+ (cdr (assoc 7 fish)) prev))
    (dolist (cell fish)
      (setf curr (cdr cell))
      (setf (cdr cell) prev)
      (setf prev curr))
    fish))

(defun simulate (fish num-days)
  (if (zerop num-days)
      fish
      (simulate (next-day fish) (1- num-days))))

;; Part 1 solution
(defun solve-1 (filename)
  (let ((fish (make-fish-list (read-file filename))))
    (reduce #'+ (simulate fish 80) :key #'cdr)))

;; Part 2 solution
(defun solve-2 (filename)
  (let ((fish (make-fish-list (read-file filename))))
    (reduce #'+ (simulate fish 256) :key #'cdr)))
