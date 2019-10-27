(add-tests-with-string-output "binary primitive +"
  [(prim-apply + 1 2)  => "3\n"]
  [(prim-apply + 1 2) => "3\n"]
  [(prim-apply + 1 -2) => "-1\n"]
  [(prim-apply + -1 2) => "1\n"]
  [(prim-apply + -1 -2) => "-3\n"]
  [(prim-apply + 536870911 -1) => "536870910\n"]
  [(prim-apply + 536870910 1) => "536870911\n"]
  [(prim-apply + -536870912 1) => "-536870911\n"]
  [(prim-apply + -536870911 -1) => "-536870912\n"]
  [(prim-apply + 536870911 -536870912) => "-1\n"]
  [(prim-apply + 1 (prim-apply + 2 3)) => "6\n"]
  [(prim-apply + 1 (prim-apply + 2 -3)) => "0\n"]
  [(prim-apply + 1 (prim-apply + -2 3)) => "2\n"]
  [(prim-apply + 1 (prim-apply + -2 -3)) => "-4\n"]
  [(prim-apply + -1 (prim-apply + 2 3)) => "4\n"]
  [(prim-apply + -1 (prim-apply + 2 -3)) => "-2\n"]
  [(prim-apply + -1 (prim-apply + -2 3)) => "0\n"]
  [(prim-apply + -1 (prim-apply + -2 -3)) => "-6\n"]
  [(prim-apply + (prim-apply + 1 2) 3) => "6\n"]
  [(prim-apply + (prim-apply + 1 2) -3) => "0\n"]
  [(prim-apply + (prim-apply + 1 -2) 3) => "2\n"]
  [(prim-apply + (prim-apply + 1 -2) -3) => "-4\n"]
  [(prim-apply + (prim-apply + -1 2) 3) => "4\n"]
  [(prim-apply + (prim-apply + -1 2) -3) => "-2\n"]
  [(prim-apply + (prim-apply + -1 -2) 3) => "0\n"]
  [(prim-apply + (prim-apply + -1 -2) -3) => "-6\n"]
  [(prim-apply + (prim-apply + (prim-apply + (prim-apply + (prim-apply + (prim-apply + (prim-apply + (prim-apply + 1 2) 3) 4) 5) 6) 7) 8) 9) => "45\n"]
  [(prim-apply + 1 (prim-apply + 2 (prim-apply + 3 (prim-apply + 4 (prim-apply + 5 (prim-apply + 6 (prim-apply + 7 (prim-apply + 8 9)))))))) => "45\n"])

(add-tests-with-string-output "binary primitive eq?"
  [(prim-apply eq? 1 1)  => "#t\n"]
  [(prim-apply eq? 1 3)  => "#f\n"])

(add-tests-with-string-output "binary primitive char=?"
  [(prim-apply char=? #\a #\a)  => "#t\n"]
  [(prim-apply char=? #\a #\z)  => "#f\n"])
