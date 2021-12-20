(defpackage :aoc2021-day20
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day20)

;; Read input
(defun read-input (filename)
  (flet ((to-bin (line)
           (map 'bit-vector (lambda (ch) (if (char= ch #\.) 0 1)) line)))

    (with-open-file (in filename)
      (let ((algo (to-bin (read-line in nil)))
            (img
              (do* ((line (read-line in nil) (read-line in nil))
                    (ret '() (cons line ret)))
                   ((null line) (mapcar #'to-bin (reverse (cdr ret)))))))

        (values algo (make-array (list (length img) (length (car img)))
                                 :element-type 'bit
                                 :initial-contents img))))))

;; Helpers
(defun pixel-code (arr x y algo flip)
  (let ((flip (and flip (= 1 (aref algo 0)))))
    (loop :for (dx dy) :in '((-1 -1) (-1 0) (-1 1)
                             ( 0 -1) ( 0 0) ( 0 1)
                             ( 1 -1) ( 1 0) ( 1 1))
          :collect (if (array-in-bounds-p arr (+ x dx) (+ y dy))
                       (aref arr (+ x dx) (+ y dy))
                       (if flip (logxor (aref algo 0) 1) (aref algo 0)))
            :into bits
          :finally (return (aref algo (reduce (lambda (a b) (+ (ash a 1) b)) bits))))))

(defun enhance (img algo &optional (rounds 1) (flip t))
  (let* ((dim (array-dimensions img))
         (n (+ 4 (car dim)))
         (m (+ 4 (cadr dim)))
         (flip (and flip (= 1 (aref algo 0))))
         (ret (make-array
               (list n m)
               :initial-element (if flip (logxor (aref algo 0) 1) (aref algo 0))
               :element-type 'bit)))

    (dotimes (x n)
      (dotimes (y m)
        (setf (aref ret x y) (pixel-code img (- x 2) (- y 2) algo flip))))

    (if (= rounds 1)
        ret
        (enhance ret algo (1- rounds) (not flip)))))

(defun count-pixels (arr)
  (let ((size (array-total-size arr)))
    (do ((i 0 (1+ i))
         (cnt 0 (+ cnt (row-major-aref arr i))))
        ((= i size) cnt))))

;; Part 1 solution
(defun solve-1 (filename)
  (multiple-value-bind (algo img) (read-input filename)
    (count-pixels (enhance img algo 2))))

;; Part 2 solution
(defun solve-2 (filename)
  (multiple-value-bind (algo img) (read-input filename)
    (count-pixels (enhance img algo 50))))
