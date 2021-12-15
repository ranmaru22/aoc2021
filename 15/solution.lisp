(defpackage :aoc2021-day15
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day15)

;; Read input
(defun read-input (filename)
  (flet ((parse-line (line)
           (mapcar #'digit-char-p (coerce line 'list))))
    
    (with-open-file (in filename)
      (do* ((line (read-line in nil) (read-line in nil))
            (ret (list (parse-line line))
                 (if line (cons (parse-line line) ret) ret)))
           ((null line)
            (make-array `(,(length ret) ,(length (car ret)))
                        :initial-contents (reverse ret)))))))

;; Helpers
(defun neighbours (arr y x)
  (loop :for (dy dx) :in '((-1 0) (1 0) (0 -1) (0 1))
        :when (array-in-bounds-p arr (+ y dy) (+ x dx))
          :collect (list (+ y dy) (+ x dx))))

(defun pop-lowest (queue)
  (let ((ret (reduce (lambda (a b) (if (< (car a) (car b)) a b)) queue)))
    (values ret (remove ret queue :test #'equal))))

;; Simple Dijkstra implementation ...
(defun make-paths (maze)
  (let* ((dim (array-dimensions maze))
         (visited (make-array dim :initial-element 'nil))
         (distances (make-array dim :initial-element most-positive-fixnum))
         (queue))

    ;; Starting point has a cost of 0
    (setf (aref distances 0 0) 0)
    
    (labels ((next (y x)
               ;; Calculate cost for all unvisited neighbours and push them into
               ;; the queue
               (loop :for (ny nx) :in (neighbours distances y x)
                     :unless (aref visited ny nx)
                       :do (let ((distance (+ (aref distances y x)
                                              (aref maze ny nx))))
                             (when (< distance (aref distances ny nx))
                               (setf (aref distances ny nx) distance)
                               (push (list distance ny nx) queue))))

               ;; Mark node as visited
               (setf (aref visited y x) t)

               (if (aref visited (1- (car dim)) (1- (cadr dim)))
                   ;; If final node has been visited, we're done
                   distances
                   ;; Otherwise pop the node with the lowest cost from the queue
                   ;; and continue
                   (multiple-value-bind (lowest rest) (pop-lowest queue)
                     (setf queue rest)
                     (next (cadr lowest) (caddr lowest))))))
      (next 0 0))))

;; Copy input array 5*5 times, increment the cost for each node by 1 for each
;; row and col added
(defun make-full-map (maze)
  (let* ((dim (array-dimensions maze))
         (rows (car dim))
         (cols (cadr dim))
         (ret (make-array `(,(* 5 rows) ,(* 5 cols)))))
    
    (loop :for i :below (* 5 rows)
          :for di := (floor i rows)
          :do (loop :for j :below (* 5 cols)
                    :for dj := (floor j cols)
                    :do (setf (aref ret i j)
                              ;; 9 wraps around to 1, so we need to do (1+x) mod 9 + 1
                              (1+ (mod (1- (+ (aref maze (mod i rows) (mod j cols)) di dj)) 9))))
          :finally (return ret))))

;; Part 1 solution
(defun solve-1 (filename)
  (let ((maze (read-input filename)))
    (destructuring-bind (n m) (array-dimensions maze)
      (aref (make-paths maze) (1- n) (1- m)))))

;; Part 1 solution
(defun solve-2 (filename)
  (let ((maze (make-full-map (read-input filename))))
    (destructuring-bind (n m) (array-dimensions maze)
      (aref (make-paths maze) (1- n) (1- m)))))
