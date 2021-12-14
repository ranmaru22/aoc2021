(defpackage :aoc2021-day12
  (:use #:cl)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day12)

;; Read input
(defun read-input (filename)
  (flet ((split-on-hyphen (line)
           (let* ((hpos (position #\- line))
                  (a (subseq line 0 hpos))
                  (b (subseq line (1+ hpos))))
             (cons a b)))
         
         (add-vertex (edge edge-list)
           (destructuring-bind (a . b) edge
             (adjoin a (adjoin b edge-list :test #'equal) :test #'equal))))

    (with-open-file (in filename)
      (do* ((line (read-line in nil) (read-line in nil))
            (edges (list (split-on-hyphen line))
                   (if (null line) edges (cons (split-on-hyphen line) edges)))
            (vertices (add-vertex (car edges) nil)
                      (add-vertex (car edges) vertices)))
           ((null line) (values vertices edges))))))

;; Helpers
(defun find-connections (vertex edges)
  (let* ((lefts (remove vertex edges :key #'car :test-not #'equal))
         (rights (remove vertex edges :key #'cdr :test-not #'equal)))
    (append (mapcar #'cdr lefts) (mapcar #'car rights))))

(defun make-graph (vertices edges)
  (mapcar (lambda (vertex) (cons vertex (find-connections vertex edges))) vertices))

(defun find-neigbours (graph vertex)
  (cdr (find vertex graph :key #'car :test #'equal)))

(defun big-cave-p (cave-name)
  (every #'upper-case-p cave-name))

(defun find-path (graph from to &key visited extra-visit (start from))
  ;; Don't add big caves to VISITED, you can visit as often as you like
  (let ((new-visited (if (big-cave-p from) visited (cons from visited))))
    (cond
      ;; If you're at the destination, return 1 for a completed path
      ((equal from to) 1)
      ;; Part 2 special: one small cave may be visited twice, so if it's already visited (and not the
      ;; start cave) but EXTRA-VISIT it T, continue anyway but set EXTRA-VISIT to NIL
      ((and extra-visit (member from visited :test #'equal) (string-not-equal from start))
       (apply #'+ (mapcar (lambda (v) (find-path graph v to :visited new-visited :extra-visit nil :start start))
                          (find-neigbours graph from))))
      ;; Return 0 for any path that leads back to a visited cave
      ((member from visited :test #'equal) 0)
      ;; Otherwise, just sum up the score for all neighbours
      (t (apply #'+ (mapcar (lambda (v) (find-path graph v to :visited new-visited :extra-visit extra-visit :start start))
                            (find-neigbours graph from)))))))

;; Part 1 solution
(defun solve-1 (filename)
  (multiple-value-bind (vertices edges) (read-input filename)
    (let ((graph (make-graph vertices edges)))
      (find-path graph "start" "end"))))

;; Part 2 solution
(defun solve-2 (filename)
  (multiple-value-bind (vertices edges) (read-input filename)
    (let ((graph (make-graph vertices edges)))
      (find-path graph "start" "end" :extra-visit t))))
