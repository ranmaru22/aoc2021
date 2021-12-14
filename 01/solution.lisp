(defpackage :aoc2021-day01
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day01)

(defun read-input (filename)
  (with-open-file (in filename)
    (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
          (ret (list line) (cons line ret)))
         ((eq line 'eof)
          (mapcar #'parse-integer (reverse (cdr ret)))))))

;; Part 1 solution
(defun calc-depth (lst &optional (ret 0))
  (if (null (cdr lst))
      ret
      (let ((fst (car lst))
            (snd (cadr lst)))
        (if (> snd fst)
            (calc-depth (cdr lst) (1+ ret))
            (calc-depth (cdr lst) ret)))))

(defun solve-1 (filename)
  (calc-depth (read-input filename)))

;; Part 2 solution
(defun calc-advanced-depth (lst &optional (ret 0) prev)
  (if (null (cddr lst))
      ret
      (let* ((fst (car lst))
             (snd (cadr lst))
             (thd (caddr lst))
             (sum (+ fst snd thd)))
        (if (and (not (null prev))
                 (> sum prev))
            (calc-advanced-depth (cdr lst) (1+ ret) sum)
            (calc-advanced-depth (cdr lst) ret sum)))))

(defun solve-2 (filename)
  (calc-advanced-depth (read-input filename)))
