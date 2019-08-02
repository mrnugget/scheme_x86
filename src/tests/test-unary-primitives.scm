(add-tests-with-string-output "prim-apply add1"
  [(prim-apply add1 1)  => "2\n"]
  [(prim-apply add1 0)  => "1\n"]
  [(prim-apply add1 -1)  => "0\n"]
  [(prim-apply add1 536870910) => "536870911\n"]
  [(prim-apply add1 -536870912) => "-536870911\n"]
  [(prim-apply add1 (prim-apply add1 0)) => "2\n"])
