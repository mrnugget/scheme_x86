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
          (let ([z 3]) (prim-apply + result z))))])
