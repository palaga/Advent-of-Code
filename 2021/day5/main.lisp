(require 'asdf)

(defun parse-coordinate (string)
  (destructuring-bind (x y)
      (mapcar #'parse-integer
       (uiop:split-string string
                          :separator ","))
    (cons x y)))

(defun parse-line (line)
  (destructuring-bind (start _ end)
      (uiop:split-string line)
    (cons
     (parse-coordinate start)
     (parse-coordinate end))))

(setq *input* (mapcar #'parse-line
               (uiop:read-file-lines "input.txt")))

(defun line-direction (line)
  (destructuring-bind ((x1 . y1) . (x2 . y2))
      line
    (cond ((eq x1 x2) 'horizontal)
          ((eq y1 y2) 'vertical))))

(defun vertical-or-horizontal-p (line)
  (case (line-direction line)
    ('horizontal t)
    ('vertical t)))

(defun add-coordinate (c1 c2)
  (destructuring-bind ((x1 . y1) . (x2 . y2))
      (cons c1 c2)
    (cons (+ x1 x2) (+ y1 y2))))

(defun subtract-coordinate (c1 c2)
  (destructuring-bind ((x1 . y1) . (x2 . y2))
      (cons c1 c2)
    (cons (- x1 x2) (- y1 y2))))

(defun negate-coordinate (coordinate)
  (destructuring-bind (x . y)
      coordinate
    (cons (- x) (- y))))

(defun normalize-coordinate (coordinate)
  (destructuring-bind (x . y)
      coordinate
    (flet ((normalize-value (n)
             (if (eq n 0)
                 0
                 (/ n (abs n)))))
      (cons (normalize-value x)
          (normalize-value y)))))


(defun delta-coordinates (c1 c2)
  (normalize-coordinate
   (negate-coordinate
    (subtract-coordinate c1 c2))))

(defun points-on-line (line)
  (destructuring-bind (from . to)
      line
    (loop with delta = (delta-coordinates from to)
          for point = from
            then (add-coordinate point delta)
          collect point
          until (equal point to))))

(let* ((vertical-and-horizontal-lines
         (remove-if-not #'vertical-or-horizontal-p
                        *input*))
       (dotted-lines
         (mapcar #'points-on-line
                 vertical-and-horizontal-lines)))
  (declare (optimize (debug 3)))
  (loop with multiples = '()
        with all-points = '()
        for points in dotted-lines
        do (break)
        do (loop
             for point in points
             if (member point all-points)
               do (push point multiples)
             else
               do (push point all-points))
        finally (return (remove-duplicates multiples))))
