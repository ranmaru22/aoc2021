(defpackage :aoc2021-day13
  (:use #:cl))

(in-package :aoc2021-day13)

;; Read input
(defun read-input (filename)
  (labels ((parse-instruction (str)
             (let ((pos (position #\Space str :from-end t)))
               (when pos (read-from-string (subseq str (1+ pos))))))
           
           (split-on (ch line)
             (let* ((pos (position ch line))
                    (a (when pos (subseq line 0 pos)))
                    (b (when pos (subseq line (1+ pos))))
                    (instruction-p (parse-instruction a)))
               (when (and a b)
                 (cons (or instruction-p (parse-integer a))
                       (parse-integer b))))))

    (with-open-file (in filename)
      (do* ((line (read-line in nil) (read-line in nil))
            (points (list (split-on #\, line))
                    (cons (split-on #\, line) points))
            (instructions (list (split-on #\= line))
                          (cons (split-on #\= line) instructions)))
           ((null line) (values
                         (remove nil points)
                         (reverse (remove nil instructions))))))))

;; Helpers
(defun fold-paper (fold-point paper)
  (let ((axis (car fold-point))
        (value (cdr fold-point)))

    (remove-duplicates
     (loop :for (px . py) :in paper
           :collect
           (cons
            (if (and (eq axis 'x)
                     (> px value))
                (- (* 2 value) px)
                px)
            (if (and (eq axis 'y)
                     (> py value))
                (- (* 2 value) py)
                py)))
     :test #'equal)))

(defun parse-folds (instructions paper)
  (if (endp instructions)
      paper
      (parse-folds (cdr instructions)
                   (fold-paper (car instructions) paper))))

(defun print-points (points)
  (let ((max-x (apply #'max (mapcar #'car points)))
        (max-y (apply #'max (mapcar #'cdr points))))
    
    (loop :for y :from 0 :upto max-y
          :do (let ((nth-points (remove y points :key #'cdr :test-not #'=)))
                (loop :for x :from 0 :upto max-x
                      :do (format t "~:[ ~;█~]" (member x nth-points :key #'car))
                      :finally (terpri))))))


;; Part 1 solution
(defun solve-1 (filename)
  (multiple-value-bind (paper instructions) (read-input filename)
    (length (fold-paper (car instructions) paper))))

;; Part 2 solution
(defun solve-2 (filename)
  (multiple-value-bind (paper instructions) (read-input filename)
    (print-points (parse-folds instructions paper))))
