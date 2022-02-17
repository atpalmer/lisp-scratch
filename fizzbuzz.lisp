(defconstant +start+ 1)
(defconstant +stop+ 101)

(defstruct hash-entry
    key
    value)

(defmacro init-hash (var entries)
    `(progn
        (setq ,var (make-hash-table))
        (dolist (e ,entries)
            (setf
                (gethash
                    (hash-entry-key e) ,var)
                    (hash-entry-value e)))))

(init-hash *str-vals* (list
    (make-hash-entry :key 3 :value "fizz")
    (make-hash-entry :key 5 :value "buzz")))

(defun is-div (i v)
    (= (mod i v) 0))

(defmacro div-cond (i by)
    `(if (is-div ,i ,by) (gethash ,by ,*str-vals*) ""))

(defun create-fizzbuzz (i)
    (concatenate 'string
        (div-cond i 3)
        (div-cond i 5)))

(defun empty-to-nil (val)
    (if (string= "" val) nil val))

(do ((i +start+ (1+ i)))
    ((= i +stop+))
    (format t "~a~%"
        (or (empty-to-nil (create-fizzbuzz i)) i)))

