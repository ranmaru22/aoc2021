(defpackage :aoc2021-day10
  (:use #:cl
        #:iterate)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day10)

;; Read input
(defun read-input (filename)
  (uiop:read-file-lines filename))

;; Helpers
(defun count-pairs (line)
  (let ((pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>))))
    (flet ((openp (ch) (member ch (mapcar #'car pairs))))
      (iter
        (for ch :in-string line)
        (with openers)
        
        (if (openp ch)
            (push ch openers)
            
            (let ((match (car (rassoc ch pairs))))
              (cond
                ((char= match (car openers)) (pop openers))
                ((char= ch #\)) (leave 3))
                ((char= ch #\]) (leave 57))
                ((char= ch #\}) (leave 1197))
                ((char= ch #\>) (leave 25137)))))
        
        (finally (return (values 0 openers)))))))

(defun complete-line (lst)
  (let ((values '((#\( . 1) (#\[ . 2) (#\{ . 3) (#\< . 4))))
    (labels ((rec (lst &optional (ret 0))
               (if (endp lst) ret
                   (rec (cdr lst) (+ (* 5 ret) (cdr (assoc (car lst) values)))))))
      (rec lst))))

;; Part 1 solution
(defun solve-1 (filename)
  (reduce #'+ (mapcar #'count-pairs (read-input filename))))

;; Part 2 solution
(defun solve-2 (filename)
  (flet ((get-scores (line)
           (complete-line (nth-value 1 (count-pairs line)))))
    (let ((scores (remove 0 (sort (mapcar #'get-scores (read-input filename)) #'<))))
      (nth (/ (1- (length scores)) 2) scores))))
