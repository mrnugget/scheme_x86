(add-tests-with-string-output "lambdas"
  [((lambda (x) (prim-apply + 1 x)) 2) => "3\n"])
