(defpackage :aoc2021-day17
  (:use #:cl)
  (:export #:solve))
(in-package :aoc2021-day17)

(defun next-pos (pos v)
  (let* ((x (car pos))
         (y (cadr pos))
         (dx (car v))
         (dy (cadr v))
         (new-pos (list (+ x dx) (+ y dy)))
         (new-v (list (cond
                        ((plusp dx) (1- dx))
                        ((minusp dx) (1+ dx))
                        (t dx))
                      (1- dy))))
    
    (list new-pos new-v)))

(defun in-target-p (pos target)
  (destructuring-bind (x y) pos
    (destructuring-bind (x0 y0 x1 y1) target
      (and (<= x0 x x1)
           (<= y0 y y1)))))

(defun overshotp (pos v target)
  (destructuring-bind (x y) pos
    (destructuring-bind (dx dy) v
      (destructuring-bind (x0 y0 x1 y1) target
        (or (and (plusp dx) (> x x1))
            (and (minusp dx) (< x x0))
            (and (minusp dy) (< y y0)))))))


(defun will-hit (v target &optional (pos '(0 0)) (max-y 0))
  (unless (overshotp pos v target)
    (let ((new-max-y (max max-y (cadr pos))))
      (if (in-target-p pos target)
          new-max-y
          (destructuring-bind (new-pos new-v) (next-pos pos v)
            (will-hit new-v target new-pos new-max-y))))))

(defun find-max (target)
  (loop :for dx :from -500 :upto 500
        :with ret
        :with vs
        :do (loop :for dy :from -500 :upto 500
                  :for max-y := (will-hit (list dx dy) target)
                  :when max-y
                    :do (push max-y ret)
                        (push (list dx dy) vs))
        :finally (return (values
                          (apply #'max ret)
                          (length (remove-duplicates vs :test #'equal))))))

;; Part 1 & 2 solution
(defun solve ()
  (find-max '(143 -106 177 -71)))
