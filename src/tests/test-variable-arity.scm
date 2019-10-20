
(add-tests-with-string-output "lambdas with varargs"
  [((lambda (a b . c) (prim-apply pair? c)) 1 2 3) => "#t\n"]
  [((lambda (a b . c) (prim-apply car c)) 1 2 3) => "3\n"]
  [((lambda (a b . c) (prim-apply cdr c)) 1 2 3) => "()\n"])

(add-tests-with-stderr-output "lambdas with varargs and wrong number of args"
  [((lambda (a b . c) (prim-apply + a b)) 1) => "Exception in system: wrong number of arguments\n"]
  [((lambda (a b . c) (prim-apply + a b)) 1 2) => ""])
