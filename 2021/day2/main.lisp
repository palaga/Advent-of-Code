(require 'uiop)

(setq *input* (mapcar #'parse-integer
                      (uiop:read-file-lines "input.txt")))

;;; Part 1
(defun number-of-increases (input)
  (loop for previous = (car input) then current
        for current in (cdr input)
        counting (> current previous) into increases
        finally (return increases)))

(number-of-increases *input*)

;;; Part 2
(defun windowed-sum (input)
  (loop for (a b c) on input
        when c
          collect (+ a b c)))

(number-of-increases
 (windowed-sum *input*))
