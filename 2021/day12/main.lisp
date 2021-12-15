(require 'asdf)

(proclaim '(optimize (debug 3) (speed 0) (space 0)))

(setq *input*
      (mapcar
       (lambda (line)
         (mapcar
          (lambda (name)
            (let ((symbol (intern name)))
              (case symbol
                ('|start| 'start)
                ('|end| 'end)
                (t symbol))))
          (uiop:split-string line :separator "-")))
       (uiop:read-file-lines "input.txt")))

(defun small-cave-p (vertex)
  (every #'lower-case-p
         (string vertex)))

(defun find-transitions (edges vertex)
  (loop for edge in edges
        for left = (car edge)
        for right = (cadr edge)
        when (eq left vertex)
          collect right into transitions
        when (eq right vertex)
          collect left into transitions
        finally (return transitions)))

(defun find-paths (edges vertex &optional path)
  (let ((new-path (cons vertex path)))
    (cond
      ((eq vertex 'start) (list new-path))
      ((and (small-cave-p vertex)
            (member vertex path))
       nil)
      (t (apply
          #'nconc
          (mapcar
           (lambda (v)
             (find-paths edges v new-path))
           (find-transitions edges vertex)))))))

(setq *paths* (find-paths *input* 'end))

(defun find-small-caves (path)
  (remove-if-not #'small-cave-p path))

(length *paths*)

