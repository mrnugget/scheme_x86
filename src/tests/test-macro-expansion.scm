(add-tests-with-string-output "let*"
  [(let* ([x 1] [y 2]) (prim-apply + x y)) => "3\n"]
  [(let* ([result (let* ([x 1] [y 2]) (prim-apply + x y))]
          [z 3])
     (prim-apply + result z)) => "6\n"])

(add-tests-with-precompiled-output "let* to let"
  [(let* ([x 1]) (prim-apply + x y))
   => (labels
        ()
        ()
        (let ((x 1))
          (prim-apply + x y)))]

  [(let* ([x 1] [y 2]) (prim-apply + x y))
   => (labels
        ()
        ()
        (let ((x 1))
          (let ((y 2))
            (prim-apply + x y))))]

  [(let* ([result (let* ([x 1] [y 2]) (prim-apply + x y))]
          [z 3])
     (prim-apply + result z))
   => (labels
        ()
        ()
        (let ([result (let ([x 1]) (let ([y 2]) (prim-apply + x y)))])
          (let ([z 3]) (prim-apply + result z))))]

  [(lambda (z) (prim-apply + z (let* ([x 1] [y 2]) (prim-apply + x y))))
   => (labels
        ((label_0
           (code (z)
                 ()
                 (prim-apply + z (let ([x 1]) (let ([y 2]) (prim-apply + x y)))))))
        ()
        (closure label_0))])

(add-tests-with-string-output "and"
  [(and (prim-apply zero? 0) (prim-apply eq? 3 4)) => "#f\n"]
  [(and (prim-apply zero? 0) (prim-apply eq? 9 9)) => "#t\n"])

(add-tests-with-precompiled-output "boolean 'and' to if expressions"
  [(and (prim-apply zero? 0))
   => (labels
        ()
        ()
        (prim-apply zero? 0))]

  [(and (prim-apply zero? 0) (prim-apply eq? 3 4))
   => (labels
        ()
        ()
        (if (prim-apply zero? 0)
            (prim-apply eq? 3 4)
            #f))]

  [(and (prim-apply zero? 0) (prim-apply eq? 3 4) (prim-apply eq? 9 8))
   => (labels
        ()
        ()
        (if (prim-apply zero? 0)
            (if (prim-apply eq? 3 4)
                (prim-apply eq? 9 8)
                #f)
            #f))])

(add-tests-with-string-output "letrec"
  [(letrec () 12) => "12\n"]
  [(letrec ([f 12]) f) => "12\n"]
  [(letrec ([f 12] [g 13]) (prim-apply + f g)) => "25\n"]
  [(letrec ([f 12] [g (lambda () f)])
     (g)) => "12\n"]
  [(letrec ([f 12] [g (lambda (n) (set! f n))])
    (g 130)
    f) => "130\n"]
  [(letrec ([f (lambda (g) (let ([x 99]) (set! f g) (f)))])
     (f (lambda () 12))) => "12\n"]

  [(letrec ([f (lambda (f n)
                  (if (prim-apply zero? n)
                      0
                      (prim-apply + n (f f (prim-apply sub1 n)))))])
      (f f 5)) => "15\n"]
  [(letrec ([user-length (lambda (lst) (if (prim-apply null? lst)
                                           0
                                           (prim-apply add1 (user-length (prim-apply cdr lst)))))])
     (user-length '(1 2 3))) => "3\n"])
