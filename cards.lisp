
(defclass deck ()
    ((cards :initform nil)))

(defmethod initialize-instance :after ((deck deck) &key)
    (let (  (cards (make-array 52 :fill-pointer 0))
            (seed (make-random-state t)))
        (dotimes (n 52)
            (vector-push n cards))
        (dotimes (n 51)
            (rotatef
                (elt cards n)
                (elt cards (+ n (random (- 52 n) seed)))))
        (setf (slot-value deck 'cards) cards)))

(defmethod deck-cards ((deck deck))
    (slot-value deck 'cards))

(defmethod deck-draw ((deck deck) &optional &key (count 1))
    (let ((result (make-array count :fill-pointer 0)))
        (dotimes (n count)
            (vector-push (vector-pop (slot-value deck 'cards)) result))
        result))

(defclass card ()
    ((value :initarg :value)))

(defmethod card-rank ((self card))
    (mod (slot-value self 'value) 13))

(defmethod card-suit ((self card))
    (floor (/ (slot-value self 'value) 13)))

(defmethod card-rank-sym ((self card))
    (elt "23456789TJQKA" (card-rank self)))

(defmethod card-suit-sym ((self card))
    (elt "shdc" (card-suit self)))

(defmethod print-object ((self card) stream)
    (format stream "~c~c"
        (card-rank-sym self)
        (card-suit-sym self)))

(defun make-card (value)
    (make-instance 'card :value value))

(defun main ()
    (let ((deck (make-instance 'deck)))
        (let ((hand (deck-draw deck :count 5)))
            (pprint (map 'vector #'make-card hand)))))

(main)

