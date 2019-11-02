(add-tests-with-string-output "let*"
  [(let* ([x 1] [y 2]) (prim-apply + x y)) => "3\n"]
  [(let* ([result (let* ([x 1] [y 2]) (prim-apply + x y))]
          [z 3])
     (prim-apply + result z)) => "6\n"])

(add-tests-with-precompiled-output "let* to let"
  [(let* ([x 1]) (prim-apply + x x))
   => (labels
        ()
        ()
        (let ((x 1))
          (prim-apply + x x)))]

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
        (let ([result (let ([x_1 1]) (let ([y_1 2]) (prim-apply + x_1 y_1)))])
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
  [(and #t) => "#t\n"]
  [(and #f) => "#f\n"]
  [(and #t #f) => "#f\n"]
  [(and #t #t) => "#t\n"]
  [(and #f #t) => "#f\n"]
  [(and #f #f) => "#f\n"])

(add-tests-with-string-output "or"
  [(or #t) => "#t\n"]
  [(or #f) => "#f\n"]
  [(or #t #t) => "#t\n"]
  [(or #f #t) => "#t\n"]
  [(or #t #f) => "#t\n"]
  [(or #f #f) => "#f\n"])

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

(add-tests-with-string-output "user-defined string=?"
  [((lambda ()
      (letrec ([rec (lambda (index)
                      (if (prim-apply eq? index 10)
                          #t
                          (rec (prim-apply add1 index))))])
        (rec 0)))) => "#t\n"]
  [(let ([user-string-eq (lambda (s1 s2)
                           (letrec ([rec (lambda (index)
                                           (if (prim-apply eq? index (prim-apply string-length s1))
                                               #t
                                               (if (prim-apply
                                                     char=?
                                                     (prim-apply string-ref s1 index)
                                                     (prim-apply string-ref s2 index))
                                                   (rec (prim-apply add1 index))
                                                   #f)))])
                             (and (prim-apply string? s1)
                                  (prim-apply string? s2)
                                  (prim-apply
                                    eq?
                                    (prim-apply string-length s1)
                                    (prim-apply string-length s2))
                                  (rec 0))))])
     (user-string-eq "foobar" "foobar")) => "#t\n"])

(add-tests-with-precompiled-output "tagging builtins with prim-apply"
  [(zero? 0) => (labels () () (prim-apply zero? 0))]
  [(let ([x 1]) (zero? x)) => (labels () () (let ([x 1]) (prim-apply zero? x)))])

;; TODO: in order for this to work, we need to extend the `identifier?` check
;; in macro expansion to wrap the builtin in a
;; `(lambda (<unique-param-names>) (prim-apply <builtin-name> <unique-param-names))`
;; For that to work though, we need to somewhere define how many params each builtin has
(add-tests-with-string-output "passing builtin functions around"
  [((lambda (f) (f 1 2)) +) => "3\n"])
