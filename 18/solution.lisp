(defpackage :aoc2021-day18
  (:use #:cl)
  (:export #:solve))
(in-package :aoc2021-day18)

(defun read-input (filename)
  (flet ((parse-line (line)
           (read-from-string
            (substitute #\( #\[ (substitute #\) #\] (substitute #\Space #\, line))))))
    
    (with-open-file (in filename)
      (do* ((line (read-line in nil) (read-line in nil))
            (ret (list line) (cons line ret)))
           ((null line) (mapcar #'parse-line (reverse (cdr ret))))))))

(defun add-left (node n)
  "Adds a value to the leftmost atom of NODE."
  (if (atom node)
      (+ node n)
      (list (add-left (car node) n) (cadr node))))

(defun add-right (node n)
  "Adds a value to the rightmost atom of NODE."
  (if (atom node)
      (+ node n)
      (list (car node) (add-right (cadr node) n))))

(defun explode (node &optional (depth 0))
  "Explodes NODE if necessary, returns it unmodified if not."
  (if (atom node)
      ;; If node is a number, return just that.
      (list node 0 0 nil)

      ;; If it's a list, step in and increase depth.
      (destructuring-bind (left ll lr lexplodedp)
          (explode (car node) (1+ depth))
        (destructuring-bind (right rl rr rexplodedp)
            (explode (cadr node) (1+ depth))

          (if (>= depth 4)
              ;; At depth >= 4, replace the list with 0 and pass the values back
              ;; up to add them to other places. Return T as the last item to
              ;; indicate that the node has been exploded.
              (list 0 left right t)

              ;; At depths < 4, add the values passed from below to the sublists
              ;; and return the updated lists.
              ;; RL is the value from the right node that has to added to the
              ;; rightmost element of the left node, and vice-versa LR is the
              ;; element from the left node that has to be added to the leftmost
              ;; element of the right node.
              (if (and lexplodedp rexplodedp)
                  (destructuring-bind (right rl rr rexplodep)
                      (explode (add-left (cadr node) lr) 4)
                    (list (list (add-right left rl) 0)
                          ll rr nil))
                  ;; Special case when both nodes were exploded: in that case, the
                  ;; left explosion needs to be taken into account when calculating
                  ;; the result for the right explosion.
                  ;; E.g. ((1 1) (2 2)) -> (0 (3 2)) -> (3 0)
                  (list (list (add-right left rl)
                              (add-left right lr))
                        ll rr nil)))))))

(defun split (node)
  "Splits NODE if necessary, returns it unmodified if not."
  (if (atom node)
      ;; If node is a number and bigger than 9, split it.
      (if (> node 9)
          (list (floor node 2) (ceiling node 2))
          node)
      ;; If it's a list, check the list elements.
      (list (split (car node))
            (split (cadr node)))))

(defun sreduce (node)
  "Reduces NODE by calling `explode' and `split' in order."
  (split (car (explode node))))

(defun add (a b)
  "Adds A to B according to snailfish maths."
  (sreduce (list a b)))

(defun magnitude (node)
  "Calculates the magnitude of NODE."
  (if (atom node)
      node
      (+ (* 3 (magnitude (car node)))
         (* 2 (magnitude (cadr node))))))

;; Part 1 solution
(defun solve-1 (filename)
  (labels ((rec (num lst)
             (if (endp lst)
                 (progn (print num) (magnitude num))
                 (rec (add num (car lst)) (cdr lst)))))

    (let ((nums (read-input filename)))
      (rec (car nums) (cdr nums)))))
