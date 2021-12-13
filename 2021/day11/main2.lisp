(require 'asdf)

()

(setq *input* (mapcar
               (lambda (l)
                 (map 'list
                  (lambda (c)
                    (parse-integer (string c)))
                  l))
               (uiop:read-file-lines "input.txt")))

(setq *field* (make-array
               (list (length *input*)
                     (length (car *input*)))
               :initial-contents *input*))

(setf  (row-major-aref *field* 20) nil)
*field*
