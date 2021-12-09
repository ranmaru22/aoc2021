(ql:quickload "iterate")
(use-package :iterate)

(defparameter *rows* nil)
(defparameter *cols* nil)

;; Read input
(defun read-input (filename)
  (with-open-file (in filename)
    (flet ((to-array (lst)
             (setf *rows* (length lst))
             (setf *cols* (length (car lst)))
             (make-array `(,*rows* ,*cols*) :initial-contents lst))
           (parse-line (line)
             (mapcar #'digit-char-p (coerce line 'list))))
      (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
            (ret (list line) (cons line ret)))
           ((eq line 'eof) (to-array (mapcar #'parse-line (reverse (cdr ret)))))))))

;; Helpers
(defun resolve (heightmap points)
  (mapcar
   (lambda (coords)
     (aref heightmap (car coords) (cdr coords)))
   points))

(defun find-neighbours (row col)
  (iter
    (for y from (1- row) to (1+ row) by 2)
    (for x from (1- col) to (1+ col) by 2)
    
    (unless (or (minusp y)
                (>= y *rows*))
      (collect (cons y col) into coordinates))
    
    (unless (or (minusp x)
                (>= x *cols*))
      (collect (cons row x) into coordinates))
    
    (finally (return coordinates))))

(defun lowest-point-p (heightmap row col)
  (let ((value (aref heightmap row col))
        (neighbours (resolve heightmap (find-neighbours row col))))
    (every (lambda (n) (< value n)) neighbours)))

(defun collect-low-points (heightmap)
  (iter outer (for row from 0 below *rows*)
    (iter (for col from 0 below *cols*)
      (when (lowest-point-p heightmap row col)
        (in outer (collect (cons row col)))))))

(defun make-tree (heightmap row col)
  (let ((seen (list (cons row col))))
    (labels ((rec (heightmap row col)
               (iter (for (x . y) in (find-neighbours row col))

                 (unless (or (member (cons x y) seen :test #'equal)
                             (= (aref heightmap x y) 9))
                   (push (cons x y) seen)
                   (collect (rec heightmap x y) into ret))

                 (finally (return (cons (cons row col) ret))))))
      
      (rec heightmap row col))))

(defun flatten-tree (tree)
  (cond
    ((null tree) nil)
    ((not (listp (cdr tree))) (list tree))
    (t (iter (for node in tree)
         (appending (flatten-tree node))))))

(defun build-basins (heightmap)
  (iter (for (x . y) in (collect-low-points heightmap))
    (collect (make-tree heightmap x y) into trees)
    (finally (return
               (mapcar (lambda (points) (resolve heightmap points))
                       (mapcar #'flatten-tree trees))))))

(defun max3 (lst &optional ret)
  (if (or (endp lst)
          (= (length ret) 3))
      ret
      (let* ((max (apply #'max lst))
             (rest (remove max lst :count 1)))
        (max3 rest (cons max ret)))))

;; Part 1 solution
(defun solve (filename)
  (let ((heightmap (read-input filename)))
    (apply #'+ (mapcar #'1+ (resolve heightmap (collect-low-points heightmap))))))

;; Part 2 solution
(defun solve-2 (filename)
  (let ((basins (build-basins (read-input filename))))
    (reduce #'* (max3 (mapcar #'length basins)))))
