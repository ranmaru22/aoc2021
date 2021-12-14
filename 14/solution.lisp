(defpackage :aoc2021-day14
  (:use #:cl
        #:str))

(in-package :aoc2021-day14)

;; Helpers
(defun add-or-inc (key alist &optional (by 1))
  (let ((entry (assoc key alist :test #'equal)))
    (if entry
        (progn (rplacd entry (+ (cdr entry) by))
               alist)
        (acons key (* 1 by) alist))))

(defun expand (pair pairs)
  (let* ((match (cadr (assoc (car pair) pairs :test #'equal)))
         (new-pair-1 (concat (subseq (car pair) 0 1) match))
         (new-pair-2 (concat match (subseq (car pair) 1))))
    (list new-pair-1 new-pair-2)))

(defun gen-template (template pairs &optional ret)
  (if (endp template)
      ret
      (let ((polymer (car template))
            (count (cdar template))
            (rest (cdr template)))
        (destructuring-bind (a b) (expand polymer pairs)
          (gen-template rest pairs (add-or-inc a (add-or-inc b ret count) count))))))

(defun grow-polymer (template pairs &optional (n 1))
  (if (zerop n)
      template
      (let ((new-template (gen-template template pairs)))
        (grow-polymer new-template pairs (1- n)))))

(defun count-polymers (template &optional ret)
  (if (endp template)
      ret
      (let* ((polymer (subseq (caar template) 1))
             (rest (cdr template))
             (count (cdar template)))
        (count-polymers rest (add-or-inc polymer ret count)))))

;; Read input
(defun read-input (filename)
  (labels ((split-pairs (str &optional ret)
             (let ((key (subseq str 0 2)))
               (if (= (length str) 2)
                   (add-or-inc key ret)
                   (split-pairs (subseq str 1) (add-or-inc key ret))))))
    
    (with-open-file (in filename)
      (let* ((first-line (read-line in nil))
             (first-char (subseq first-line 0 1))
             (template (split-pairs first-line)))
        (do* ((line (read-line in nil) (read-line in nil))
              (pairs (list (split " -> " line :omit-nulls t))
                     (cons (split " -> " line :omit-nulls t) pairs)))
             ((null line) (values template (remove nil pairs) first-char)))))))

;; Part 1 & 2 solution
(defun solve (filename n)
  (multiple-value-bind (template pairs first-char) (read-input filename)
    (let* ((counts (count-polymers (grow-polymer template pairs n)))
           (counts* (add-or-inc first-char counts))
           (mce (reduce #'max counts* :key #'cdr))
           (lce (reduce #'min counts* :key #'cdr)))
      (- mce lce))))
