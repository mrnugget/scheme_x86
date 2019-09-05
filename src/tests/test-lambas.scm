(add-tests-with-string-output "lambdas"
  [(prim-apply procedure? (lambda (x) (prim-apply + x x))) => "#t\n"])
