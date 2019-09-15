(add-tests-with-precompiled-output "lambdas to labels"
  ;; no free vars
  [((lambda (x) (prim-apply + x x)) 3)
  => (labels
       ((label_0 (code (x) () (prim-apply + x x))))
       ()
       (funcall (closure label_0) 3))]
  ;; single free var
  [(let ((y 4)) ((lambda (x) (prim-apply + y x)) 3))
  => (labels
       ((label_0 (code (x) (y) (prim-apply + y x))))
       ()
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
       ()
       (let ([y 4] [foobar 99])
         (funcall (closure label_1 y) 3)
         (funcall (closure label_2 y) 5)
         (funcall (closure label_0 foobar) 1)))])


(add-tests-with-precompiled-output "tailcall annotation"
  [(let ([g (lambda (x) (prim-apply + x x))])
     ((lambda (y) (g y)) 5))
  => (labels
       ((label_1 (code (y) (g) (tailcall g y)))
        (label_0 (code (x) () (prim-apply + x x))))
       ()
       (let ([g (closure label_0)])
         (funcall (closure label_1 g) 5)))]

  [(let ([g (lambda (x) (prim-apply + x x))])
     ((lambda (y) (if (prim-apply zero? y) 0 (g y))) 5))
  => (labels
       ((label_1
          (code (y) (g) (if (prim-apply zero? y) 0 (tailcall g y))))
        (label_0 (code (x) () (prim-apply + x x))))
       ()
       (let ([g (closure label_0)])
         (funcall (closure label_1 g) 5)))]

  [(let ([g (lambda (x) (other-func x x))]
     (g 5)))
  => (labels
       ((label_0
          (code (x) (other-func) (tailcall other-func x x))))
       ()
       (let ([g (closure label_0 other-func)] [g 5])))])

(add-tests-with-precompiled-output "constants to labels"
  [(let ((f (lambda () (quote (1 . "H")))))
     (eq? (f) (f)))
   =>
   (labels ((label_0 (datum))
            (label_1 (code () () (constant-ref label_0))))
          ((constant-init label_0 (prim-apply cons 1 (string #\H))))
          (let ((f (closure label_1)))
            (funcall eq? (funcall f) (funcall f))))]
  )
