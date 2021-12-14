(defpackage :aoc2021-day08
  (:use #:cl
        #:split-sequence)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day08)

;; Read input
(defun read-file (filename)
  (with-open-file (in filename)
    (labels ((sort-chars (s) (sort s #'char-lessp))
             (split-and-sort (s)
               (mapcar #'sort-chars (split-sequence #\Space s :remove-empty-subseqs t))))
      
      (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
            (ret))
           ((eq line 'eof) (reverse ret))     
        (setf ret (cons (mapcar #'split-and-sort (split-sequence #\| line)) ret))))))

;; Helpers
(defun find-prime-signals (lst &optional ret rem)
  (if (endp lst)
      (values ret rem)
      (let ((sign (car lst))
            (rest (cdr lst)))
        (case (length sign)
          (2 (find-prime-signals rest (cons `(1 . ,sign) ret) rem))
          (3 (find-prime-signals rest (cons `(7 . ,sign) ret) rem))
          (4 (find-prime-signals rest (cons `(4 . ,sign) ret) rem))
          (7 (find-prime-signals rest (cons `(8 . ,sign) ret) rem))
          (otherwise (find-prime-signals rest ret (cons sign rem)))))))

(defun contains-prime-p (sign prime primes)
  (let* ((sign (coerce sign 'list))
         (prime (coerce (cdr (assoc prime primes)) 'list))
         (num-intersections (length (intersection sign prime))))
    (values (= num-intersections (length prime))
            num-intersections)))

(defun decode-signals (lst primes &optional ret)
  (if (endp lst)
      (append primes ret)
      (let ((sign (car lst))
            (rest (cdr lst)))
        (cond
          ;;; LENGTH 5 SIGNALS
          ;; 3 -> contains 1
          ((and (= (length sign) 5)
                (contains-prime-p sign 1 primes))
           (decode-signals rest primes (cons `(3 . ,sign) ret)))

          ;; 2 -> 2 intersections with 4
          ((and (= (length sign) 5)
                (= (nth-value 1 (contains-prime-p sign 4 primes)) 2))
           (decode-signals rest primes (cons `(2 . ,sign) ret)))

          ;; 5 -> 3 intersections with 4
          ((and (= (length sign) 5)
                (= (nth-value 1 (contains-prime-p sign 4 primes)) 3))
           (decode-signals rest primes (cons `(5 . ,sign) ret)))

          ;;; LENGTH 6 SIGNALS
          ;; 9 -> contains 4
          ((and (= (length sign) 6)
                (contains-prime-p sign 4 primes))
           (decode-signals rest primes (cons `(9 . ,sign) ret)))

          ;; 0 -> contains 1 but not 4
          ((and (= (length sign) 6)
                (contains-prime-p sign 1 primes))
           (decode-signals rest primes (cons `(0 . ,sign) ret)))

          ;; 6 -> contains neither 1 nor 4 
          ((and (= (length sign) 6))
           (decode-signals rest primes (cons `(6 . ,sign) ret)))
          
          (t (decode-signals rest primes ret))))))

(defun count-unique-digits (lst &optional (ret 0))
  (if (endp lst)
      ret
      (let ((primes (find-prime-signals (caar lst)))
            (output (cadar lst))
            (count 0))
        (dolist (value output)
          (when (member value primes :key #'cdr :test #'equal)
            (incf count)))
        (count-unique-digits (cdr lst) (+ ret count)))))

(defun decode (output code-table &optional (ret 0))
  (if (endp output)
      ret
      (decode (cdr output) code-table
              (+ (* (car (rassoc (car output) code-table :test #'equal))
                    (expt 10 (1- (length output))))
                 ret))))

;; Part 1 solution
(defun solve-1 (filename)
  (let ((patterns (read-file filename)))
    (count-unique-digits patterns)))

;; Part 2 solution
(defun solve-2 (filename)
  (let ((patterns (read-file filename))
        (sum 0))
    (dolist (pattern patterns)
      (multiple-value-bind (primes composites) (find-prime-signals (car pattern))
        (setf sum (+ sum (decode (cadr pattern) (decode-signals composites primes))))))
    sum))
