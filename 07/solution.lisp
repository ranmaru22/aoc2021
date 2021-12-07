(ql:quickload "split-sequence")

;; Read input
(defun read-file (filename)
  (with-open-file (in filename)
    (sort (mapcar #'parse-integer (split-sequence:split-sequence #\, (read-line in nil))) #'<)))

;; Helpers
(defun median (lst)
  (let ((i (floor (length lst) 2)))
    (nth i lst)))

(defun diff-to-target-pos (lst target-pos)
  (mapcar (lambda (x) (abs (- x target-pos))) lst))

(defun crab-fuel (lst target-pos)
  (let ((diff (diff-to-target-pos lst target-pos)))
    (mapcar (lambda (x) (/ (* x (+ x 1)) 2)) diff)))

;; Part 1 solution
(defun solve (filename)
  (let ((crabs (read-file filename)))
    (reduce #'+ (diff-to-target-pos crabs (median crabs)))))

;; Part 2 solution
(defun solve-2 (filename)
  (let ((crabs (read-file filename))
        (checked-positions)
        (fuel-calculations))
    (dolist (pos crabs)
      (unless (member pos checked-positions)
        (push pos checked-positions)
        (push (reduce #'+ (crab-fuel crabs pos)) fuel-calculations)))
    (apply #'min fuel-calculations)))
