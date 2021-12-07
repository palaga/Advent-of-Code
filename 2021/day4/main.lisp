(require 'asdf)

(setq *input*
      (uiop:read-file-lines "input.txt"))

(setq *bingo-numbers*
      (mapcar #'parse-integer
              (uiop:split-string
               (pop *input*)
               :separator ",")))

(setq *boards*
      (flet ((parse-line (line)
               (remove-if
                #'null
                (mapcar
                 (lambda (n)
                   (when (not (string= n ""))
                     (cons (parse-integer n)
                           nil)))
                 (uiop:split-string line)))))
        (loop with board = '()
              for line in *input*

              if (string= line "")
                collect board into boards
                and do (setq board '())
              else
                do (setq board
                         (append board
                                 (list (parse-line line))))
              finally (return (cdr boards)))))

;;; Part 1
(defun mark-number (board number)
  (mapcar
   (lambda (row)
     (mapcar
      (lambda (tuple)
        (if (eq (car tuple) number)
            (cons number t)
            tuple))
      row))
   board))

(defun transpose (m)
  (apply #'mapcar #'list m))

(defun winner-p (board)
  (flet ((bingo-in-row (row)
           (every #'cdr row))
         (transpose (m)
           (apply #'mapcar #'list m)))
    (or (some #'bingo-in-row board)
        (some #'bingo-in-row (transpose board)))))

(defun find-winner (boards)
  (loop for board in boards
        when (winner-p board)
          return board))

(defun sum-of-unmarked-numbers (board)
  (flet ((sum-unmarked-row (row)
           (reduce
            #'+
            (mapcar
             #'car
             (remove-if #'cdr row)))))
    (reduce
     (lambda (acc row)
       (+ acc (sum-unmarked-row row)))
     board
     :initial-value 0)))

(defun play-round (boards number)
  (mapcar
   (lambda (board)
     (mark-number board number))
   boards))

(defun play-game (boards numbers)
  (loop for number in numbers
        for rest on numbers
        for game-state = (play-round boards number)
          then (play-round game-state number)
        for winner = (find-winner game-state)
          then (find-winner game-state)
        when winner
          return (list
                  (cons winner
                        (remove-if
                         #'winner-p
                         game-state))
                  rest)))


(defun calculate-score (boards numbers)
  (* (car numbers)
     (sum-of-unmarked-numbers (car boards))))

(apply #'calculate-score
       (play-game *boards* *bingo-numbers*))

;;; Part 2

(defun find-last-winner (boards numbers &optional winner)
  (if (not numbers)
      winner
      (let* ((new-winner (play-game boards numbers))
             (rest-boards (cdar new-winner))
             (rest-numbers (cdadr new-winner)))
        (print (length
                rest-boards))
        (print (length rest-numbers))
        (find-last-winner
           rest-boards
           rest-numbers
           (or new-winner winner)))))

(apply #'calculate-score
       (find-last-winner *boards* *bingo-numbers*)       )

