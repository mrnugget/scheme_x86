(add-tests-with-precompiled-output "let* to let"
  [(let* ([x 1]
          [y 2])
     (prim-apply + x y))
  => (labels
       ()
       ()
       (let ((x 1))
         (let ((y 2))
           (prim-apply + x y))))])
