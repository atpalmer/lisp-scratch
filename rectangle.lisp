
;;; Classes

(defclass rectangle () (
    (width
        :initarg :width
        :reader width)
    (height
        :initarg :height
        :reader height)))

(defmethod area ((self rectangle))
    (with-slots (width height) self
        (* width height)))

(defmethod print-object ((self rectangle) stream)
    (print-unreadable-object (self stream)
        (format stream "Shape: ~a (Area: ~a x ~a = ~a)"
            (type-of self) (width self) (height self) (area self))))

(defclass square (rectangle) (
    (width :initarg :side)
    (height :initarg :side)))

(let
    (   (s (make-instance 'square :side 5))
        (r (make-instance 'rectangle :width 45 :height 123)))
    (format t "~a~%" s)
    (format t "~a~%" r))


;;; Structs

(defstruct struct-rect
    width
    height)

(defun struct-rect-area (r)
    (* (struct-rect-width r) (struct-rect-height r)))

; structs are actually classes,
; so we can still defmethod print-object
(defmethod print-object ((r struct-rect) stream)
    (print-unreadable-object (r stream)
        (format stream "Shape: ~a (Area: ~a x ~a = ~a)"
            (type-of r) (struct-rect-width r) (struct-rect-height r) (struct-rect-area r))))

(defun make-square (&key side)
    (make-struct-rect :width side :height side))

(let
    (   (s (make-square :side 5))
        (r (make-struct-rect :width 45 :height 123)))
    (format t "~a~%" s)
    (format t "~a~%" r))

