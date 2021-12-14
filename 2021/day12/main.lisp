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

(defun small-cave-p (cave)
  (every #'lower-case-p
         (string cave)))

(defun find-transitions (edges vertex)
  (loop for edge in edges
        for left = (car edge)
        for right = (cdr edge)
        when (eq left vertex)
          collect right into transitions
        when (eq right vertex)
          collect left into transitions
        finally (return transitions)))
(find-transitions *input* 'start)
