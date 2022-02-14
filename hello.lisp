(setq welcome "hello")
(setq name "world")

(format t "~a"
    (string-capitalize
        (string-trim "!"
            (concatenate 'string welcome ", " name "!!!"))))
