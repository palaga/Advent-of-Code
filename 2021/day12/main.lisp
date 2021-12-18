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

(defun find-paths (edges)
  (flet ((visited-small-cave-p (vertex stack)
           (and (small-cave-p vertex)
                (member vertex stack
                        :key #'car)))
         (cleanup-stack (stack)
           (member-if-not #'null stack :key #'cdr)))
    (loop
      for stack = `((end . ,(find-transitions edges 'end)))
        then (cleanup-stack stack)
      while stack
      for vertex = (caar stack)
      if (eq vertex 'start)
        collect (mapcar #'car stack) into paths
        and do (pop stack)
      else
        do (let ((new-vertex (pop (cdar stack))))
             (unless (or (eq new-vertex 'end)
                      (visited-small-cave-p new-vertex stack))
                 (push
                  `(,new-vertex . ,(find-transitions edges new-vertex))
                  stack)))
      finally (return paths))))

(setq *paths* (find-paths *input*))

(length *paths*)

;;; Part 2
