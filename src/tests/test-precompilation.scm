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
     (prim-apply eq? (f) (f)))
   =>
   (labels
     ((label_0 (datum))
      (label_1 (code () () (constant-ref label_0))))
     ((constant-init
        label_0
        (prim-apply
          cons
          1
          (let ([s (prim-apply make-string 1)])
            (prim-apply string-set! s 0 #\H)
            s))))
     (let ([f (closure label_1)])
       (prim-apply eq? (funcall f) (funcall f))))])

(add-tests-with-precompiled-output "set! to vector-set/ref"
  [(let ([f (lambda (c)
              (prim-apply cons (lambda (v) (set! c v))
                          (lambda () c)))])
    (let ([p (f 0)]) ((prim-apply car p) 12) ((prim-apply cdr p))))
  =>
  (labels
     ((label_2
         (code (c)
               ()
               (let ([c (let ([v (prim-apply make-vector 1)])
                           (prim-apply vector-set! v 0 c)
                           v)])
                  (prim-apply
                     cons
                     (closure label_0 c)
                     (closure label_1 c)))))
        (label_1 (code () (c) (prim-apply vector-ref c 0)))
        (label_0 (code (v) (c) (prim-apply vector-set! c 0 v))))
     ()
     (let ([f (closure label_2)])
        (let ([p (funcall f 0)])
           (funcall (prim-apply car p) 12)
           (funcall (prim-apply cdr p)))))])

(add-tests-with-precompiled-output "all precompilations in one"
  [(let ([g (lambda (x) (prim-apply + x x))]
        [f (lambda () (quote (1 . "H")))])
    ((lambda (y) (g y)) 5)
    (prim-apply eq? (f) (f)))
  => (labels
       ((label_0 (datum))
        (label_3 (code (y) (g) (tailcall g y)))
        (label_2 (code () () (constant-ref label_0)))
        (label_1 (code (x) () (prim-apply + x x))))
     ((constant-init
        label_0
        (prim-apply
          cons
          1
          (let ([s (prim-apply make-string 1)])
            (prim-apply string-set! s 0 #\H)
            s))))
       (let ([g (closure label_1)] [f (closure label_2)])
         (funcall (closure label_3 g) 5)
         (prim-apply eq? (funcall f) (funcall f))))])


(add-tests-with-precompiled-output "primitive refs"
  [(addandaddfour 1 2)
  => (labels () () (funcall (primitive-ref addandaddfour) 1 2))])
