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
