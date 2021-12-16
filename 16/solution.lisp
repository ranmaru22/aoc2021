(defpackage :aoc2021-day16
  (:use #:cl #:sb-gray #:select)
  (:export #:solve-1 #:solve-2))
(in-package :aoc2021-day16)

;; Types
(defclass bit-stream (fundamental-binary-input-stream)
  ((data :initarg :data
         :type (vector bit-vector))
   (position :initform 0)))

(defmethod read-bit ((stream bit-stream))
  (with-slots (data position) stream
    (when (< position (length data))
        (prog1
            (aref data position)
          (incf position)))))

(defun hex-to-bin-str (hex-str)
  (apply #'concatenate 'string
         (mapcar (lambda (num) (format nil "~4,'0B" num))
                 (mapcar (lambda (ch) (digit-char-p ch 16)) (coerce hex-str 'list)))))

(defun bin-str-to-bit-vector (bin-str)
  (let ((bitvec (make-array (length bin-str) :element-type 'bit)))
    (loop :for i :below (length bin-str)
          :do (setf (aref bitvec i) (if (char= (char bin-str i) #\0) 0 1)))
    bitvec))

(defun make-bit-stream (hex-str)
  (make-instance 'bit-stream
                 :data (bin-str-to-bit-vector (hex-to-bin-str hex-str))))

;; Read input
(defun read-input (filename)
  (with-open-file (in filename)
    (make-bit-stream (read-line in nil))))

;; Helpers to extract paket information
(defun packet-version (pkt)
  (to-int (select pkt (range 0 3))))

(defun type-id (pkt)
  (to-int (select pkt (range 3 6))))

(defun to-int (bitvec)
  (reduce (lambda (a b) (+ (ash a 1) b)) bitvec))

;; Stream operations
(defun read-chunk (stream n)
  (loop :for i :below n
        :collect (read-bit stream)))

(defun read-packet-header (stream)
  (loop :for count :from 0 :below 6
        :for bit := (read-bit stream)
        :collect bit :into header
        :finally (return (coerce header 'bit-vector))))

(defun read-value-packet-content (stream)
  (loop :for chunk := (read-chunk stream 5)
        :for chunk-val := (select chunk (range 1 nil))
        :while (/= (car chunk) 0)
        :collect chunk :into chunks
        :collect chunk-val :into values
        :finally (return
                   (values
                    (apply #'concatenate 'bit-vector
                           (nconc chunks (list chunk)))
                    0 ; Version sum comes from header, return 0 here
                    (to-int (apply #'concatenate 'bit-vector
                                   (nconc values (list chunk-val))))))))

(defun read-op-type0-sub-packets (stream len-spec)
  (loop :with len := 0
        :while (< len (to-int len-spec))
        :for (pkt vs val) := (multiple-value-list (read-packet stream))
        :do (incf len (length pkt))
        :collect pkt :into pkts
        :sum vs :into vsum
        :collect val :into values
        :finally (return
                   (values
                    (apply #'concatenate 'bit-vector pkts)
                    vsum
                    values))))

(defun read-op-type1-sub-packets (stream len-spec)
  (loop :for count :below (to-int len-spec)
        :for (pkt vs val) := (multiple-value-list (read-packet stream))
        :collect pkt :into pkts
        :sum vs :into vsum
        :collect val :into values
        :finally (return
                   (values
                    (apply #'concatenate 'bit-vector pkts)
                    vsum
                    values))))

(defun >* (a b) (if (> a b) 1 0))

(defun <* (a b) (if (< a b) 1 0))

(defun =* (a b) (if (= a b) 1 0))

(defun read-op-packet-content (stream &optional op)
  (flet ((read-len-spec (type)
           (loop :for i :below (if (eql type 0) 15 11)
                 :collect (read-bit stream))))
    
    (let* ((len-type (read-bit stream))
           (len-spec (read-len-spec len-type)))
      
      (multiple-value-bind (content vsum values)
          (case len-type
            (0 (read-op-type0-sub-packets stream len-spec))
            (1 (read-op-type1-sub-packets stream len-spec)))

        (values
         (concatenate 'bit-vector (list len-type) len-spec content)
         vsum
         (apply op values))))))

(defun read-packet (stream)
  (let ((header (read-packet-header stream)))
    (multiple-value-bind (content vsum values)
        (case (type-id header)
          (0 (read-op-packet-content stream #'+))
          (1 (read-op-packet-content stream #'*))
          (2 (read-op-packet-content stream #'min))
          (3 (read-op-packet-content stream #'max))
          (4 (read-value-packet-content stream))
          (5 (read-op-packet-content stream #'>*))
          (6 (read-op-packet-content stream #'<*))
          (7 (read-op-packet-content stream #'=*)))

      (values
       (concatenate 'bit-vector header content)
       (+ (packet-version header) (or vsum 0))
       values))))

;; Part 1 solution
(defun solve-1 (filename)
  (let ((stream (read-input filename)))
    (nth-value 1 (read-packet stream))))

;; Part 2 solution
(defun solve-2 (filename)
  (let ((stream (read-input filename)))
    (nth-value 2 (read-packet stream))))
