(add-tests-with-precompiled-output "lambdas to labels"
  ;; no free vars
  [((lambda (x) (prim-apply + x x)) 3)
  => (labels
       ((L_0 (code (x) () (prim-apply + x x))))
       (funcall (closure L_0) 3))]
  ;; single free var
  [(let ((y 4)) ((lambda (x) (prim-apply + y x)) 3))
  => (labels
       ((L_0 (code (x) (y) (prim-apply + y x))))
       (let ([y 4]) (funcall (closure L_0 y) 3)))]
  ;; single free var in multiple lambdas
  [(let ((y 4) (foobar 99))
     ((lambda (x) (prim-apply + y x)) 3)
     ((lambda (z) (prim-apply + y z)) 5)
     ((lambda (a) (prim-apply + a foobar)) 1))
  => (labels
       ((L_2 (code (z) (y) (prim-apply + y z)))
        (L_1 (code (x) (y) (prim-apply + y x)))
        (L_0 (code (a) (foobar) (prim-apply + a foobar))))
       (let ([y 4] [foobar 99])
         (funcall (closure L_1 y) 3)
         (funcall (closure L_2 y) 5)
         (funcall (closure L_0 foobar) 1)))])
