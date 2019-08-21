(add-tests-with-string-output "if expressions"
  [(if #t 12 13) => "12\n"]
  [(if #f 12 13) => "13\n"]
  [(if 0 12 13)  => "12\n"]
  [(if () 43 ()) => "43\n"]
  [(if #t (if 12 13 4) 17) => "13\n"]
  [(if #f 12 (if #f 13 4)) => "4\n"]
  [(if #\X (if 1 2 3) (if 4 5 6)) => "2\n"]
  [(if (if (prim-apply char? #\a) (prim-apply boolean? #\b) (prim-apply fixnum? #\c)) 119 -23) => "-23\n"]
  [(if (prim-apply char? 12) 13 14) => "14\n"]
  [(if (prim-apply char? #\a) 13 14) => "13\n"]
  [(prim-apply add1 (if (prim-apply sub1 1) (prim-apply sub1 13) 14)) => "13\n"])
