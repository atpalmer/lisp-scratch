

(defstruct triangle
    base
    height)

(defmethod print-object ((self triangle) stream)
    (let ((height (triangle-height self)) (base (triangle-base self)))
        (loop for level from 1 to height
            do (loop for _ from 1 to (* level (/ base height))
                do (write "." :stream stream))
            (terpri stream))))

(let ((tri (make-triangle :base 40 :height 20)))
    (format t "~a" tri))

