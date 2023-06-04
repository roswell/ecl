
(block foobar
  (print "HELLO")
  (if (= x 3)
      (return-from foobar (identity :returned-value))
      (print "CONTINUE"))
  (print "GOOD BYE"))
