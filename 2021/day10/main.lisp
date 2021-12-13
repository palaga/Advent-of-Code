(require 'asdf)

(setq *input* (uiop:read-file-lines "input.txt")
      *opening-brackets* "([{<"
      *closing-brackets* ")]}>"
      *brackets* (map 'list #'cons *opening-brackets* *closing-brackets*)
      *scores* (map 'list #'cons *closing-brackets* '(3 57 1197 25137)))

(defun opening-p (bracket)
  (find bracket *opening-brackets*))

(defun matching-p (opening closing)
  (eq (cdr (assoc opening *brackets*))
      closing))

(defun find-mismatch (line)
  (with-input-from-string (str line)
    (loop with stack = '()
          for c = (read-char str nil nil)
          while c
          if (opening-p c)
            do (push c stack)
          else
            unless (matching-p (pop stack) c)
              return c)))

(defun score (closing-bracket)
  (cdr (assoc closing-bracket *scores*)))

(reduce
 #'+
 (mapcar
  #'score
  (remove
   nil
   (mapcar #'find-mismatch *input*))))

;;; Part 2
(defun get-closing-brackets (line)
  (with-input-from-string (str line)
    (loop with stack = '()
          for c = (read-char str nil nil)
          while c
          if (opening-p c)
            do (push c stack)
          else
            do (pop stack)
          finally
             (return
               (coerce
                (mapcar
                 (lambda (b)
                   (cdr (assoc b *brackets*)))
                 stack)
                'string)))))

(defun auto-complete-score (bracket)
  (1+ (position bracket *closing-brackets*)))

(defun gather-scores (brackets)
  (map 'list #'auto-complete-score brackets))

(defun final-score (scores)
  (reduce
   (lambda (acc n)
     (+ (* 5 acc) n))
   scores
   :initial-value 0))

(defun brackets-to-sorted-scores (brackets &key (pred #'<))
 (sort
  (mapcar
   #'final-score
   (mapcar
    #'gather-scores brackets))
  pred))

(let* ((incomplete-lines (remove-if #'find-mismatch *input*))
       (closing-brackets (mapcar #'get-closing-brackets
                                 incomplete-lines))
       (results (brackets-to-sorted-scores
                 closing-brackets
                 :pred #'<))
       (index (floor (/ (length results) 2))))
  (nth index results))
