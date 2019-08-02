(add-tests-with-string-output "prim-apply add1"
  [(prim-apply add1 1)  => "2\n"]
  [(prim-apply add1 0)  => "1\n"]
  [(prim-apply add1 -1)  => "0\n"]
  [(prim-apply add1 536870910) => "536870911\n"]
  [(prim-apply add1 -536870912) => "-536870911\n"]
  [(prim-apply add1 (prim-apply add1 0)) => "2\n"])

(add-tests-with-string-output "prim-apply sub1"
  [(prim-apply sub1 2)  => "1\n"]
  [(prim-apply sub1 1)  => "0\n"]
  [(prim-apply sub1 0)  => "-1\n"]
  [(prim-apply sub1 -1)  => "-2\n"]
  [(prim-apply sub1 536870911) => "536870910\n"]
  [(prim-apply sub1 -536870911) => "-536870912\n"]
  [(prim-apply sub1 (prim-apply sub1 2)) => "0\n"])

(add-tests-with-string-output "prim-apply fixnum->char and prim-apply char->fixnum"
   [(prim-apply fixnum->char 65) => "#\\A\n"]
   [(prim-apply fixnum->char 97) => "#\\a\n"]
   [(prim-apply fixnum->char 122) => "#\\z\n"]
   [(prim-apply fixnum->char 90) => "#\\Z\n"]
   [(prim-apply fixnum->char 48) => "#\\0\n"]
   [(prim-apply fixnum->char 57) => "#\\9\n"]
   [(prim-apply char->fixnum #\A) => "65\n"]
   [(prim-apply char->fixnum #\a) => "97\n"]
   [(prim-apply char->fixnum #\z) => "122\n"]
   [(prim-apply char->fixnum #\Z) => "90\n"]
   [(prim-apply char->fixnum #\0) => "48\n"]
   [(prim-apply char->fixnum #\9) => "57\n"]
   [(prim-apply char->fixnum (prim-apply fixnum->char 12)) => "12\n"]
   [(prim-apply fixnum->char (prim-apply char->fixnum #\x)) => "#\\x\n"])


(add-tests-with-string-output "prim-apply zero?"
   [(prim-apply zero? 0) => "#t\n"]
   [(prim-apply zero? 1) => "#f\n"]
   [(prim-apply zero? -1) => "#f\n"]
   [(prim-apply zero? 64) => "#f\n"]
   [(prim-apply zero? 960) => "#f\n"])
