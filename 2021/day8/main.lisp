(require 'asdf)

(defun parse-line (line)
  (labels ((trim-spaces (string)
             (string-trim " " string))
           (parse-sequence (string)
             (uiop:split-string
              (trim-spaces string))))
    (mapcar #'parse-sequence
            (uiop:split-string
             line
             :separator "|"))))

(setq *input* (mapcar #'parse-line
                      (uiop:read-file-lines "input.txt")))

(setq *unique-counts-map* '((1 . 2) (4 . 4)
                            (7 . 3) (8 . 7)))

(defun get-lengths (list)
  (mapcar #'length list))

(let ((unique-counts (mapcar #'cdr *unique-counts-map*)))
  (flet ((unique-segments-p (n)
           (member n unique-counts))
         (get-output-lengths (entry)
           (get-lengths (cadr entry))))
    (let* ((output-lengths (mapcan #'get-output-lengths
                                   *input*))
           (segments-with-unique-length (mapcar #'unique-segments-p output-lengths)))
      (length
       (remove-if #'null segments-with-unique-length)))))

;;; Part 2
(defun decode-unique (signals)
  (mapcar
   (lambda (s)
     (let ((result (car
                    (rassoc
                     (length s) *unique-counts-map*))))
       (cons s result)))
   signals))

(defun string-set-binop (op string1 string2)
  (funcall op
    (coerce string1 'list)
    (coerce string2 'list)))

(defun string-union (string1 string2)
  (coerce
   (string-set-binop #'union
                     string1 string2)
   'string))

(defun string-difference (string1 string2)
  (coerce
   (string-set-binop #'set-difference
                     string1 string2)
   'string))

(defun string-equal-set (string1 string2)
  (null
   (string-set-binop #'set-exclusive-or
                     string1 string2)))

(defun string-subset-p (string1 string2)
  (string-set-binop #'subsetp
                     string1 string2))

(defun decode-non-unique (table)
  (labels ((segments-to-digit (s)
             (cdr (assoc s table)))
           (digit-to-segments (d)
             (car (rassoc d table)))
           (filter-table (p)
             (mapcar 'car (remove-if-not p table)))
           (set-segment (string n)
             (setf (cdr (assoc string table)) n))
           (car-filter-table (p)
             (car (filter-table p))))
    (let* ((six-segments (car-filter-table
                          (lambda (pair)
                            (and
                             (string-subset-p (string-difference
                                               (digit-to-segments 8)
                                               (digit-to-segments 7))
                                              (car pair))
                             (not (cdr pair))))))
           (five-segments (car-filter-table
                           (lambda (pair)
                             (and (eq (length (car pair)) 5)
                                  (eq (length (string-difference six-segments
                                                                 (car pair)))
                                      1)))))
           (nine-segments (car-filter-table
                           (lambda (pair)
                             (string-equal-set
                              (string-union
                               five-segments
                               (digit-to-segments 1))
                              (car pair)))))
           (zero-segments (car-filter-table
                           (lambda (pair)
                             (let ((signal (car pair)))
                               (and (eq (length signal) 6)
                                    (not (eq signal six-segments))
                                    (not (eq signal nine-segments)))))))
           (three-segments (car-filter-table
                            (lambda (pair)
                              (eq (length (string-difference (car pair)
                                                             (digit-to-segments 1)))
                                  3))))
           (two-segments (car-filter-table
                          (lambda (pair)
                            (let ((signal (car pair)))
                              (and (eq (length signal) 5)
                                   (not (eq signal three-segments))
                                   (not (eq signal five-segments))))))))
      (set-segment six-segments 6)
      (set-segment five-segments 5)
      (set-segment nine-segments 9)
      (set-segment zero-segments 0)
      (set-segment three-segments 3)
      (set-segment two-segments 2)
      table)))

(defun digits-to-number (digits)
  (parse-integer
   (format nil "~{~a~}" (mapcar #'write-to-string digits))))

(defun calculate-output-digits (entry)
  (let ((decoded-signals (decode-non-unique
                          (decode-unique (car entry))))
        (outputs (cadr entry)))
    (mapcar
     (lambda (item)
       (loop for pair in decoded-signals
          when (string-equal-set (car pair) item)
          return (cdr pair)))
     outputs)))

;;; Answer
(apply #'+
       (mapcar
        (lambda (entry)
          (digits-to-number (calculate-output-digits entry)))
        *input*))
