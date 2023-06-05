
(block foobar
  (print "HELLO")
  (unwind-protect
       (if (= x 3)
           (return-from foobar (identity :returned-value))
           (print "CONTINUE"))
    (do-something)
    (print "E"))
  (print "GOOD BYE"))
