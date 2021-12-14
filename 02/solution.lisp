(defpackage :aoc2021-day02
  (:use #:cl)
    (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day02)

(defun line-to-cons (line)
  (cons (read-from-string line)
        (read-from-string (subseq line (position #\Space line)))))

(defun read-input (filename)
  (with-open-file (in filename)
    (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
          (ret (list line) (cons line ret)))
         ((eq line 'eof) (mapcar #'line-to-cons (reverse (cdr ret)))))))

;; Part 1 solution
(defun get-position (instructions &optional (h 0) (d 0))
  (if (endp instructions)
      (* h d)
      (let ((next (car instructions)))
        (case (car next)
          (forward (get-position (cdr instructions) (+ h (cdr next)) d))
          (up (get-position (cdr instructions) h (- d (cdr next))))
          (down (get-position (cdr instructions) h (+ d (cdr next))))))))

(defun solve-1 (filename)
  (get-position (read-input filename)))

;; Part 2 solution
(defun get-position* (instructions &optional (h 0) (d 0) (aim 0))
  (if (endp instructions)
      (* h d)
      (let ((next (car instructions)))
        (case (car next)
          (forward (get-position* (cdr instructions) (+ h (cdr next)) (+ d (* (cdr next) aim)) aim))
          (up (get-position* (cdr instructions) h d (- aim (cdr next))))
          (down (get-position* (cdr instructions) h d (+ aim (cdr next))))))))

(defun solve-2 (filename)
  (get-position* (read-input filename)))
