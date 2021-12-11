(defpackage :aoc2021-day11
  (:use #:cl))

(in-package :aoc2021-day11)

;; Read input
(defun read-input (filename)
  (flet ((parse-line (line)
           (mapcar #'digit-char-p (coerce line 'list))))
    
    (with-open-file (in filename)
      (do* ((line (read-line in nil) (read-line in nil))
            (ret (list (parse-line line)) (cons (parse-line line) ret)))
           ((null line)

            (let ((n (length (cdr ret)))
                  (m (length (cadr ret))))
              (make-array (list n m) :initial-contents (reverse (cdr ret)))))))))

;; Helpers
(defmacro loop-array (arr &body body)
  `(let* ((dim (array-dimensions ,arr))
          (n (1- (car dim)))
          (m (1- (cadr dim))))
     (do* ((i 0 (if (= j m) (1+ i) i))
           (j 0 (if (= j m) 0 (1+ j))))
          ((= i (1+ n)) ,arr)
       (symbol-macrolet ((el (aref ,arr i j)))
         ,@body))))

(defun indices (pred arr)
  (let ((ret))
    (loop-array arr
      (when (funcall pred el) (push (cons i j) ret)))
    ret))

(defun adjacents (arr row col)
  (let ((ret))
    (dolist (drow '(-1 0 1))
      (dolist (dcol '(-1 0 1))
        (when (and (not (= drow dcol 0))
                   (array-in-bounds-p arr (+ row drow) (+ col dcol)))
          (push (cons (+ row drow) (+ col dcol)) ret))))
    ret))

(defun bump-energy-levels (octopodes &optional positions)
  (loop-array octopodes
    (when (or (null positions)
              (member (cons i j) positions :test #'equal))
      (setf el (1+ el)))))

(defun reset (octopodes)
  (let ((dim (array-dimensions octopodes))
        (count 0))
    (loop-array octopodes
      (when (> el 9)
        (setf el 0)
        (incf count)))
    (values count (= count (apply #'* dim)))))

(defun flash (octopodes &optional exclude)
  (let* ((ready-to-flash (set-difference (indices (lambda (x) (> x 9)) octopodes) exclude :test #'equal))
         (affecteds (mapcar (lambda (x) (adjacents octopodes (car x) (cdr x))) ready-to-flash)))
    (if (null ready-to-flash)
        octopodes
        (progn
          (mapcan (lambda (positions) (bump-energy-levels octopodes positions)) affecteds)
          (flash octopodes (append ready-to-flash exclude))))))

(defun next-step (octopodes &optional (num 1) (num-flashes 0) syncp)
  (if (zerop num)
      (values octopodes num-flashes syncp)
      (multiple-value-bind (count syncp) (reset (flash (bump-energy-levels octopodes)))
        (next-step octopodes (1- num) (+ num-flashes count) syncp))))

;; Part 1 solution
(defun solve-1 (filename)
  (next-step (read-input filename) 100))

;; Part 2 solution
(defun solve-2 (filename)
  (do ((octopodes (read-input filename))
       (count 0 (1+ count))
       (syncp nil (nth-value 2 (next-step octopodes))))
      (syncp count)))
