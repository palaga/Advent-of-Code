(require 'asdf)

;;; Read bit strings as integers
(setq *input*
      (uiop:read-file-lines "input.txt"))

;;; Part 1
(defun bit-to-delta (char)
  "Map '1' to 1 and '0' to -1."
  (case char
    (#\1 1)
    (#\0 -1)))

(defun bits-to-deltas (line)
  (map 'vector #'bit-to-delta line))

(defun vector-add (v1 v2)
  "Add the numbers in v1 to those in v2."
  (map 'vector #'+ v1 v2))

(defun delta-to-bit (delta)
  (cond ((> delta 0) #\1)
        ((< delta 0) #\0)))

(defun deltas-to-bits (deltas)
  "Convert positive numbers to '1' and negative numbers to '0'."
  (map 'string #'delta-to-bit deltas))

(defun invert-bit (bit)
  (case bit
    (#\1 #\0)
    (#\0 #\1)))

(defun invert-bits (bits)
  (map 'string #'invert-bit bits))

(defun reduce-deltas (input)
  (loop for line in *input*
        for result = (bits-to-deltas line)
          then (vector-add result (bits-to-deltas line))
        finally (return result)))

(let* ((gamma-bits (deltas-to-bits (reduce-deltas *input*)))
       (epsilon-bits (invert-bits gamma-bits))
       (gamma-rate (parse-integer gamma-bits :radix 2))
       (epsilon-rate (parse-integer epsilon-bits :radix 2)))
  (* gamma-rate epsilon-rate))


;;; Part 2
(defun most-common-bit (index candidates)
  "Most common bit at index among candidates."
  (loop for candidate in candidates
        summing (bit-to-delta
                 (elt candidate index))
          into result
        finally (return (delta-to-bit result))))

(defun find-rating (candidates
                    &key
                      (index 0)
                      (remove-fn #'remove-if-not)
                      (tie-breaker #\1))
  (declare (optimize (speed 0) (debug 3)))
  (if (not (cdr candidates))
      (car candidates)
      (find-rating
       (let ((mc (or (most-common-bit index candidates) tie-breaker)))
         (funcall remove-fn
                (lambda (candidate)
                  (eq mc (elt candidate index)))
                candidates))
       :index (1+ index)
       :remove-fn remove-fn
       :tie-breaker tie-breaker)))

(let ((oxygen-generator-rating (find-rating *input*))
      (co2-scrubber-rating (find-rating *input* :remove-fn #'remove-if)))
  (* (parse-integer oxygen-generator-rating :radix 2)
     (parse-integer co2-scrubber-rating :radix 2)))
