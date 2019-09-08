(add-tests-with-string-output "lambdas"
  [((lambda (x) x) 99) => "99\n"]
  [(prim-apply closure? (lambda (x) x)) => "#t\n"]
  [(prim-apply closure? 1) => "#f\n"]
  [(prim-apply closure? (prim-apply make-vector 0)) => "#f\n"]
  [(prim-apply closure? (prim-apply make-string 0)) => "#f\n"]
  [((lambda (x) (prim-apply + 1 x)) 2) => "3\n"]
  [((lambda (x y) (prim-apply + x y)) 2 3) => "5\n"]
  [((lambda (a b c d e) (prim-apply + a (prim-apply + b (prim-apply + c (prim-apply + d e))))) 1 2 3 4 5) => "15\n"]
  [((lambda (g) (g)) (lambda () 99)) => "99\n"]
  [(let ([f (lambda (g) (g 2 13))])
    (f (lambda (n m) (prim-apply + n m)))) => "15\n"]
  [((lambda (g x) (g x)) (lambda (x) (prim-apply + 1 x)) 2) => "3\n"])
