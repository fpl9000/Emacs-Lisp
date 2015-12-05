(cond ((= 1 2)
       (insert "foo\n")
       (insert "bar\n"))

      ((= 3 3)
       (insert "hello\n")
       (insert "world\n"))

      (t
       (insert "default\n")))
