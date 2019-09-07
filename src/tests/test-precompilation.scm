(add-tests-with-precompiled-output "lambdas to labels"
  ;; no free vars
  [((lambda (x) (prim-apply + x x)) 3)
  => (labels
       ((label_0 (code (x) () (prim-apply + x x))))
       (funcall (closure label_0) 3))]
  ;; single free var
  [(let ((y 4)) ((lambda (x) (prim-apply + y x)) 3))
  => (labels
       ((label_0 (code (x) (y) (prim-apply + y x))))
       (let ([y 4]) (funcall (closure label_0 y) 3)))]
  ;; single free var in multiple lambdas
  [(let ((y 4) (foobar 99))
     ((lambda (x) (prim-apply + y x)) 3)
     ((lambda (z) (prim-apply + y z)) 5)
     ((lambda (a) (prim-apply + a foobar)) 1))
  => (labels
       ((label_2 (code (z) (y) (prim-apply + y z)))
        (label_1 (code (x) (y) (prim-apply + y x)))
        (label_0 (code (a) (foobar) (prim-apply + a foobar))))
       (let ([y 4] [foobar 99])
         (funcall (closure label_1 y) 3)
         (funcall (closure label_2 y) 5)
         (funcall (closure label_0 foobar) 1)))])
