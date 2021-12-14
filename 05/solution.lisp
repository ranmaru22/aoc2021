(defpackage :aoc2021-day05
  (:use #:cl
        #:split-sequence)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day05)

(defparameter *dimensions* 1000)

;; Read input
(defun read-file (filename)
  (with-open-file (in filename)

    (flet ((split-cell (line)
             (let* ((first-pair-end (position #\Space line))
                    (second-pair-start (+ first-pair-end 4)))
               (cons (mapcar #'parse-integer
                             (split-sequence #\, (subseq line 0 first-pair-end)))
                     (list (mapcar #'parse-integer
                                   (split-sequence #\, (subseq line second-pair-start))))))))
      
      (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
            (ret))
           ((eq line 'eof) (reverse ret))
        (setf ret (cons (split-cell line) ret))))))

;; Helpers
(defun destructure-point (cell)
  (values (caar cell) (cadar cell) (caadr cell) (cadadr cell)))

(defun draw-lines (point-list &key (field (make-array `(,*dimensions* ,*dimensions*))) ignore-diagonals)
  (if (endp point-list)
      field

      (multiple-value-bind (x1 y1 x2 y2) (destructure-point (car point-list))
        (unless (and ignore-diagonals
                     (/= x1 x2)
                     (/= y1 y2))          
          (do ((i x1 (cond ((> i x2) (1- i))
                           ((< i x2) (1+ i))
                           (t i)))
               (j y1 (cond ((> j y2) (1- j))
                           ((< j y2) (1+ j))
                           (t j))))
              ((and (= i x2)
                    (= j y2))
               (incf (aref field j i)))
            (incf (aref field j i))))
        
        (draw-lines (cdr point-list) :ignore-diagonals ignore-diagonals :field field))))

(defun count-danger-points (field)
  (let ((count 0))
    (dotimes (i (* *dimensions* *dimensions*))
      (when (> (row-major-aref field i) 1)
        (incf count)))
    count))

;; Part 1 solution
(defun solve-1 (filename)
  (count-danger-points (draw-lines (read-file filename) :ignore-diagonals t)))

;; Part 2 solution
(defun solve-2 (filename)
  (count-danger-points (draw-lines (read-file filename))))
