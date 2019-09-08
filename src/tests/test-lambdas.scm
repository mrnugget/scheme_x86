(add-tests-with-string-output "lambdas"
  [((lambda (x) x) 99) => "99\n"]
  [((lambda (x)
      (prim-apply + 1 x))
    2) => "3\n"]
  [((lambda (x y)
      (prim-apply + x y))
    2
    3) => "5\n"]
  [((lambda (a b c d e)
      (prim-apply +
                  a
                  (prim-apply +
                              b
                              (prim-apply +
                                          c
                                          (prim-apply + d e)))))
    1 2 3 4 5) => "15\n"]

  [(lambda (x) x) => "<closure>\n"]

  [(prim-apply closure? (lambda (x) x)) => "#t\n"]
  [(prim-apply closure? 1) => "#f\n"]
  [(prim-apply closure? (prim-apply make-vector 0)) => "#f\n"]
  [(prim-apply closure? (prim-apply make-string 0)) => "#f\n"]

  [((lambda (g) (g)) (lambda () 99)) => "99\n"]
  [(let ([f (lambda (g) (g 2 13))])
    (f (lambda (n m) (prim-apply + n m)))) => "15\n"]
  [((lambda (g x) (g x)) (lambda (x) (prim-apply + 1 x)) 2) => "3\n"]
  [(let ([f (lambda (g) (prim-apply + (g 10) (g 100)))])
    (f (lambda (x) (prim-apply + x x)))) => "220\n"])

(add-tests-with-string-output "closures"
 [(let ([n 12])
    (let ([f (lambda () n)])
      (f))) => "12\n"]
 [(let ([n 12])
    (let ([f (lambda (m) (prim-apply + n m))])
      (f 100))) => "112\n"]
 [(let ([f (lambda (f n m)
             (if (prim-apply zero? n)
                 m
                 (f (prim-apply sub1 n) (prim-apply + n m))))])
   (let ([g (lambda (g n m) (f (lambda (n m) (g g n m)) n m))])
     (g g 5 1))) => "16\n"]
 [(let ([f (lambda (f n)
             (if (prim-apply zero? n)
                 1
                 (prim-apply + n (f (prim-apply sub1 n)))))])
   (let ([g (lambda (g n) (f (lambda (n) (g g n)) n))])
     (g g 5))) => "16\n"]
)
