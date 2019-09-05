(add-tests-with-precompiled-output "lambdas to labels"
  ;; no free vars
  [((lambda (x) (prim-apply + x x)) 3)
  => (labels
       ((lbl0 (code (x) () (prim-apply + x x))))
       (funcall (closure lbl0) 3))]
  ;; single free var
  [(let ((y 4)) ((lambda (x) (prim-apply + y x)) 3))
  => (labels
       ((lbl0 (code (x) (y) (prim-apply + y x))))
       (let ([y 4]) (funcall (closure lbl0 y) 3)))]
  ;; single free var in multiple lambdas
  [(let ((y 4) (foobar 99))
     ((lambda (x) (prim-apply + y x)) 3)
     ((lambda (z) (prim-apply + y z)) 5)
     ((lambda (a) (prim-apply + a foobar)) 1))
  => (labels
       ((lbl2 (code (z) (y) (prim-apply + y z)))
        (lbl1 (code (x) (y) (prim-apply + y x)))
        (lbl0 (code (a) (foobar) (prim-apply + a foobar))))
       (let ([y 4] [foobar 99])
         (funcall (closure lbl1 y) 3)
         (funcall (closure lbl2 y) 5)
         (funcall (closure lbl0 foobar) 1)))])
