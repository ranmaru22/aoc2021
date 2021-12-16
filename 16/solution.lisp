(defpackage :aoc2021-day16
  (:use #:cl #:select)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day16)

;; Read input
(defun bin-string (hex-str)
  (apply #'concatenate 'string
         (mapcar (lambda (num) (format nil "~4,'0B" num))
                 (mapcar (lambda (ch) (digit-char-p ch 16)) (coerce hex-str 'list)))))

(defun read-input (filename)
  (with-open-file (in filename)
    (bin-string (read-line in nil))))

;; Helpers
(defun from-str (bin-str)
  (let ((bitvec (make-array (length bin-str) :element-type 'bit)))
    (loop :for i :below (length bin-str)
          :do (setf (aref bitvec i) (if (char= (char bin-str i) #\0) 0 1)))
    bitvec))

(defun to-int (bitarr)
  (reduce (lambda (a b) (+ (ash a 1) b)) bitarr))

(defun packet-version (pkg)
  (to-int (select pkg (range 0 3))))

(defun type-id (pkg)
  (to-int (select pkg (range 3 6))))

(defun op-length-id (pkg)
  (to-int (select pkg 7)))

(defun packet-contents (pkg)
  (let ((type-id (type-id pkg)))
    (case type-id
      ;; Value pakages
      (4 (loop :for i :from 6 :by 5
               :for chunk-type := (select pkg i)
               :for chunk := (select pkg (range (1+ i) (+ i 5)))
               :while (/= chunk-type 0)
               :collect chunk :into chunks
               :finally (return (to-int (apply #'concatenate 'bit-vector (nconc chunks (list chunk)))))))

      ;; Operator packages
      (otherwise ()))))
