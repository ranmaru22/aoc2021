(defpackage :aoc2021-day24
  (:use #:cl #:str)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day24)

(defmacro alu-read (str)
  `(mapcar #'read-from-string (words ,str)))

(defmacro alu-eval (op a &optional b)
  (print op) (print a) (print b)
  `(cond
     ((eq ,op 'inp) (setf ,a (read)))
     ((eq ,op 'add) (setf ,a (+ ,a ,b)))
     ((eq ,op 'mul) (setf ,a (* ,a ,b)))
     ((eq ,op 'div) (setf ,a (floor ,a ,b)))
     ((eq ,op 'mod) (setf ,a (mod ,a ,b)))
     ((eq ,op 'eql) (setf ,a (if (eql ,a ,b) 1 0)))))

(defmacro alu-parse (&body code)
  (let ((w) (x) (y) (z))
    `(alu-eval ,@code)))

(defun run-program (filename)
  (let ((w 9) (x 0) (y 0) (z 0))
    (alu-parse (from-file filename))

    (values w x y z)))
