(defpackage :aoc2021-day03
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day03)

;; Read input
(defun read-input (filename)
  (with-open-file (in filename)
    (do* ((line (read-line in nil 'eof) (read-line in nil 'eof))
          (ret (list line) (cons line ret)))
         ((eq line 'eof) (reverse (cdr ret))))))

;; Helpers
(defun analyze-bits (bit-str)
  (map 'list (lambda (s) (if (= (digit-char-p s) 0) '(1 . 0) '(0 . 1))) bit-str))

(defun cons+ (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (cons (+ (car a) (car b))
             (+ (cdr a) (cdr b))))))

(defun zip-with (fn xs ys &optional ret)
  (if (or (endp xs) (endp ys))
      (reverse ret)
      (zip-with fn (cdr xs) (cdr ys) (cons (funcall fn (car xs) (car ys)) ret))))

(defun count-bits (numbers &optional ret)
  (if (endp numbers)
      ret
      (let ((bits (analyze-bits (car numbers)))
            (ret (if (null ret) (make-list (length (car numbers))) ret)))
        (count-bits (cdr numbers) (zip-with #'cons+ bits ret)))))

(defun compare-bits (bit-list comp-fn)
  (reduce (lambda (acc n) (+ (* 2 acc) n))
          (mapcar (lambda (b) (if (funcall comp-fn (car b) (cdr b)) 0 1)) bit-list)
          :initial-value 0))

;; Part 1 solution
(defun gamma*epsilon (bits)
  (let ((gamma (compare-bits (count-bits bits) #'>))
        (epsilon (compare-bits (count-bits bits) #'<)))
    (* gamma epsilon)))

(defun solve-1 (filename)
  (gamma*epsilon (read-input filename)))

;; Part 2 solution
(defun find-life-support-rating (bits)
  (labels ((find-value (bits cmp-fn &optional (offset 0))
             (if (<= (length bits) 1)
                 (car bits)
                 (let* ((bitcount (nth offset (count-bits bits)))
                        (mcv (if (funcall cmp-fn (car bitcount) (cdr bitcount)) #\0 #\1)))
                   
                   (find-value
                    (remove-if-not (lambda (ch) (char= mcv ch))
                                   bits
                                   :key (lambda (bit) (char bit offset)))
                    cmp-fn
                    (1+ offset))))))
    
    (let ((oxygen-generator-value (find-value bits #'>))
          (co2-scrubber-value (find-value bits #'<=)))
      
      (* (parse-integer oxygen-generator-value :radix 2)
         (parse-integer co2-scrubber-value :radix 2)))))

(defun solve-2 (filename)
  (find-life-support-rating (read-input filename)))
