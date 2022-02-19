;;;;
;;;; Calculates a similarity ratio between two strings
;;;; using the "Levenshtein distance" algorithm.
;;;;


(defun make-matrix (s1 s2)
    (let ((matrix
        (make-array
            (list (+ 1 (length s1)) (+ 1 (length s2)))
                :adjustable nil
                :initial-element 0
                :element-type 'integer)))
    (dotimes (y (+ 1 (length s1)))
        (setf (aref matrix y 0) y))
    (dotimes (x (+ 1 (length s2)))
        (setf (aref matrix 0 x) x))
    matrix))

(defun cost-of-deletions (matrix x y)
    (aref matrix y (- x 1)))

(defun cost-of-insertions (matrix x y)
    (aref matrix (- y 1) x))

(defun cost-of-substitutions (matrix x y)
    (aref matrix (- y 1) (- x 1)))

(defun cost (arr x y subcost)
    (min
        (+ (cost-of-deletions arr x y) 1)
        (+ (cost-of-insertions arr x y) 1)
        (+ (cost-of-substitutions arr x y) subcost)))


(defun word-similarity (s1 s2)
    (let* (
        (matrix (make-matrix s1 s2))
        (distance
            (destructuring-bind
                    (y-len x-len)
                    (array-dimensions matrix)
                (loop for y from 1 below y-len
                    do (loop for x from 1 below x-len
                        do (let ((subcost (if (eql (elt s1 (- y 1)) (elt s2 (- x 1))) 0 2)))
                            (setf (aref matrix y x) (cost matrix x y subcost)))))
                (aref matrix (- y-len 1) (- x-len 1)))))
        (/  (- (+ (length s1) (length s2)) distance)
            (+ (length s1) (length s2)))))


(defun main ()
    (let* (
            (word1 "kitten")
            (word2 "sitting")
            (pct (word-similarity word1 word2)))
        (format t "Similarity between ~s and ~s: ~5f%~%" word1 word2 (* pct 100))))

(main)

