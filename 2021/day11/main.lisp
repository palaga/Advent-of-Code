(require 'asdf)

(proclaim '(optimize (debug 3) (speed 0) (space 0)))

(setq *input* (mapcar
               (lambda (l)
                 (map 'list
                  (lambda (c)
                    (parse-integer (string c)))
                  l))
               (uiop:read-file-lines "input.txt")))

(defun get-field ()
  (make-array
   (list (length *input*)
         (length (car *input*)))
   :initial-contents *input*))

(defun coordinate= (c1 c2)
  (destructuring-bind ((y1 . x1) (y2 . x2))
      (list c1 c2)
    (and (eq x1 x2)
         (eq y1 y2))))

(defun find-neighbors (array row col)
  (loop
    for y from (1- row) to (1+ row)
    nconc (loop for x from (1- col) to (1+ col)
                when (array-in-bounds-p array y x)
                  unless (coordinate= (cons y x)
                                      (cons row col))
                    collect (cons y x))))

(defun flash (field row col)
  (let ((value (aref field row col)))
    (when (and value (> value 9))
      (setf (aref field row col) nil)
      (mapc
       (lambda (neighbor)
         (destructuring-bind (y . x)
             neighbor
           (when (aref field y x)
             (incf (aref field y x)))
           (funcall #'flash field y x)))
       (find-neighbors field row col)))))

(defun index-to-coordinate (field index)
  (destructuring-bind (height width)
      (array-dimensions field)
    (cons (floor (/ index width)) (mod index width))))

(defun apply-flashes (field)
  (let ((size (array-total-size field)))
    (dotimes (index size)
      (destructuring-bind (y . x)
          (index-to-coordinate field index)
        (flash field y x)))

    ;; Set nil to 0
    (dotimes (index size)
      (unless (row-major-aref field index)
        (setf (row-major-aref field index) 0)))))

(defun count-zeroes (field)
  (loop for index below (array-total-size field)
        counting (zerop (row-major-aref field index))))

(defun increment-field (field)
  (dotimes (index (array-total-size field))
    (incf (row-major-aref field index))))


(defun do-step (field)
  (increment-field field)
  (apply-flashes field)
  field)

(loop
  repeat 100
  for field = (do-step (get-field))
    then (do-step field)
  summing (count-zeroes field))

;;; Part 2
(loop
  for field = (get-field)
    then (do-step field)
  until (eq (array-total-size field)
            (count-zeroes field))
  count t)
