(import 'asdf)

(let ((lines (uiop:read-file-lines "input.txt")))
  (defconstant +template+
    (car lines))
  (defconstant +transitions+
    (let ((ht (make-hash-table :test #'equal)))
      (mapc
       (lambda (transition)
         (destructuring-bind (rule -> result)
             (uiop:split-string transition)
           (setf (gethash rule ht)
                 (char result 0))))
       (cddr lines))
      ht)))

(defun do-step (template)
  (loop for i below (1- (length template))
        for key = (subseq template i (+ i 2))
        nconc (list
               (char template i)
               (gethash key +transitions+))
                 into result
        finally (return (concatenate
                         'string
                         (append
                          result
                          (list (char template i)))))))

(defun do-steps (n)
 (loop repeat n
       for result = (do-step +template+)
         then (do-step result)
       finally (return result)))

(defun count-occurrences (template)
  (loop with result = (make-hash-table)
        for c across template
        do (incf (gethash c result 0))
        finally (return result)))

(defun min-max-diff (occurrences)
  (loop
    for character being the hash-keys of occurrences
      using (hash-value count)
    maximizing count into max-count
    minimizing count into min-count
    finally (return (- max-count min-count))))

(min-max-diff
 (count-occurrences
  (do-steps 10)))

;;; Part 2
(defun count-pairs (pair-counts char-counts)
  (flet ((get-transition (pair)
           (let ((value (gethash pair +transitions+)))
             (values
              value
              (coerce `(,(char pair 0) ,value) 'string)
              (coerce `(,value ,(char pair 1)) 'string)))))
    (loop
      with new-pair-counts = (make-hash-table :test #'equal)
      for pair being the hash-keys of pair-counts
        using (hash-value count)
      do (multiple-value-bind (char left right)
             (get-transition pair)
           (incf (gethash char char-counts 0) count)
           (incf (gethash left new-pair-counts 0) count)
           (incf (gethash right new-pair-counts 0) count))
      finally (return (list new-pair-counts char-counts)))))

(let ((pair-counts (make-hash-table :test #'equal))
      (char-counts (make-hash-table))
      (template-length (length +template+)))
  (loop for i below template-length
        when (< i (1- template-length))
          do (incf (gethash (subseq +template+ i (+ i 2)) pair-counts 0))
        do (incf (gethash (char +template+ i) char-counts 0)))
  (loop
    repeat 40
    for result = (count-pairs pair-counts char-counts)
      then (apply #'count-pairs result)
    finally
       (return
        (min-max-diff char-counts))))
