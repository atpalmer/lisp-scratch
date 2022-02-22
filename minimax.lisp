;;;;
;;;; "Minimax" algorithm
;;;;
;;;; Choose "left" or "right" from top of tree to maximize result
;;;; from a given list versus another player seeking to minimize.
;;;;

(defconstant +random-limit+ 50)
(defconstant +game-depth+ 3)


(defclass puzzle () (
    (items :type 'vector :initarg :items)
    (depth :type 'fixnum :initarg :depth)))

(defun make-puzzle (&key depth)
    (let (
            (seed (make-random-state t))
            (items (make-array (expt 2 depth) :fill-pointer 0)))
        (dotimes (_ (expt 2 depth))
            (vector-push (random +random-limit+ seed) items))
        (make-instance 'puzzle :depth depth :items items)))


(defmacro next-left (i)
    `(* ,i 2))

(defmacro next-right (i)
    `(+ (* ,i 2) 1))

(defun minimax (puzzle i cur-depth &key me opp)
    (if (= cur-depth (slot-value puzzle 'depth))
        (elt (slot-value puzzle 'items) i)
        (funcall me
            (minimax puzzle (next-left  i) (+ cur-depth 1) :me opp :opp me)
            (minimax puzzle (next-right i) (+ cur-depth 1) :me opp :opp me))))

(defun minimax-top (puzzle)
    (let (
            (lval (minimax puzzle 0 1 :me #'max :opp #'min))
            (rval (minimax puzzle 1 1 :me #'max :opp #'min)))
        (if (> lval rval)
            (list 'left lval)
            (list 'right rval))))

(defun main ()
    (let ((puzzle (make-puzzle :depth +game-depth+)))
        (apply
            #'format t
            "Puzzle: ~a~%Best Move: ~a~%Best Result: ~a~%"
            (slot-value puzzle 'items)
            (minimax-top puzzle))))

(main)

