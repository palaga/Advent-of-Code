(require 'asdf)

(with-open-file (stream "input.txt")
  (loop
    with is-coordinate = t
    for line = (read-line stream nil)
    while line
    when (string= line "")
      do (setq is-coordinate nil
               line (read-line stream t t))
    if is-coordinate
      collect (mapcar
               #'parse-integer
               (uiop:split-string line :separator ","))
        into coordinates
    else
      collect
      (destructuring-bind (dimension value)
          (uiop:split-string
           (third
            (uiop:split-string line))
           :separator "=")
        (cons
         (intern (string-upcase dimension))
         (parse-integer value)))
      into instructions
    finally
       (setq *coordinates* coordinates
             *instructions* instructions)))

(defun find-fold-parts (coordinates dimension fold)
  (flet ((compare-to-fold (fun coord)
           (funcall fun
                    (if (eq dimension 'x)
                       (car coord)
                       (cadr coord))
                  fold)))
    (loop
      for coordinate in coordinates
      if (compare-to-fold #'< coordinate)
        collect coordinate into smaller
      else
        when (compare-to-fold #'> coordinate)
          collect coordinate into bigger
      finally (return (values smaller bigger)))))

(defun mirror-over (coordinates dimension fold)
  (mapcar
   (lambda (c)
     (if (eq dimension 'x)
         (list (- fold (- (car c) fold)) (cadr c))
         (list (car c) (- fold (- (cadr c) fold)))))
   coordinates))

(defun follow-instruction (coordinates instruction)
 (destructuring-bind (dim . value) instruction
   (multiple-value-bind (small big)
       (find-fold-parts coordinates dim value)
     (union small (mirror-over big dim value)
            :test (lambda (c1 c2)
                    (and (eq (first c1) (first c2))
                         (eq (second c1) (second c2))))))))

(length
 (follow-instruction
  *coordinates*
  (car *instructions*)))

;;; Part 2

(defun max-coordinates (coordinates)
  (loop for (x y) in coordinates
        maximizing x into max-x
        maximizing y into max-y
        finally (return (list max-x max-y))))

(defun coordinates-to-image (coordinates dimensions)
  (loop
    with image = (make-array dimensions)
    for (x y) in coordinates
    do (setf (aref image x y) t)
    finally (return image)))

(defun coordinates-to-pbm (coordinates filename)
  (uiop:with-output-file (stream filename)
    (destructuring-bind (width height)
        (mapcar #'1+ (max-coordinates coordinates))
      (format stream "P1~%~d ~d~%" width height)
      (loop with image = (coordinates-to-image
                          coordinates (list width height))
            for y below height
            do (loop for x below width
                     do (format stream "~:[0~;1~]"
                                (aref image x y)))
            do (format stream "~%")))))

(coordinates-to-pbm
 (reduce
  #'follow-instruction
  *instructions*
  :initial-value *coordinates*)
 "result.pbm")
