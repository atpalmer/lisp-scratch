(defconstant +start+ 1)
(defconstant +stop+ 101)

(setf *mod-strings* '(
    (3 . "fizz")
    (5 . "buzz")))

(defmacro string-for-mod (div i)
    `(if (= (mod ,i ,div) 0) (cdr (assoc ,div *mod-strings*)) ""))

(defun create-fizzbuzz (i)
    (concatenate 'string
        (string-for-mod 3 i)
        (string-for-mod 5 i)))

(defun empty-to-nil (val)
    (if (string= "" val) nil val))

(do ((i +start+ (1+ i)))
    ((= i +stop+))
    (format t "~a~%"
        (or (empty-to-nil (create-fizzbuzz i)) i)))

