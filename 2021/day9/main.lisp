(require 'asdf)

(defun parse-line (line)
  (map 'vector (lambda (char)
                 (parse-integer
                  (string char)))
       line))

(setq *input* (map 'vector #'parse-line
                      (uiop:read-file-lines "input.txt")))

(setq *depth-map* (let* ((row-length (length *input*))
                         (column-length (length (elt *input* 0)))
                         (dimension (list row-length column-length)))
                    (make-array dimension :initial-contents *input*)))

(defmacro defdirection (name row-diff col-diff)
  `(defun ,name (map row col &key coordinate)
     (let ((new-row (+ row ,row-diff))
           (new-col (+ col ,col-diff)))
       (when (array-in-bounds-p map new-row new-col)
         (if coordinate
             (cons new-row new-col)
             (aref map new-row new-col))))))

(defdirection up -1 0)
(defdirection down 1 0)
(defdirection left 0 -1)
(defdirection right 0 1)

(defun get-adjacent-locations (map row col)
  (remove
   nil
   (mapcar
    (lambda (direction)
      (funcall direction map row col))
    '(up down left right))))

(defun low-point-p (map row col)
  (let ((point (aref map row col))
        (adjacent-minimum
          (apply #'min (get-adjacent-locations map row col))))
    (< point adjacent-minimum)))

(defun get-low-points (map)
  (destructuring-bind (height width)
      (array-dimensions map)
    (loop
      for row below height nconc
        (loop
          for col below width
          when (low-point-p map row col)
            collect (aref map row col)))))

(reduce
 #'+ (mapcar
      #'1+ (get-low-points *depth-map*)))

;;; Part 2
(defun flood-fill (map shadow row col)
  (let ((value (aref map row col))
        (marked (aref shadow row col)))
    (flet ((recurse (direction)
             (let ((next (funcall direction map row col :coordinate t)))
               (if next
                   (flood-fill map shadow (car next) (cdr next))
                   0))))
      (setf (aref shadow row col) t)
      (if (or marked (eq value 9))
          0
          (1+
           (reduce
            #'+
            (mapcar
             #'recurse
             '(right down left up))))))))

(defun get-basins (depth-map)
  (destructuring-bind (height width)
      (array-dimensions depth-map)
    (let ((depth-map-shadow (make-array (list height width) :initial-element nil)))
      (flet ((get-state (row col) (aref depth-map-shadow row col)))
        (loop
          for row below height
          nconc
          (loop
            for col below width
            for visited = (get-state row col)
            when (not visited)
              collect (flood-fill depth-map
                                  depth-map-shadow
                                  row col)))))))

(apply
 #'*
 (subseq
  (sort
   (remove-if #'zerop (get-basins *depth-map*))
   #'>)
  0 3))
