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
