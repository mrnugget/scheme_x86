(add-tests-with-string-output "pairs"
  [(prim-apply cons 1 2) => "(1 . 2)\n"]
  [(prim-apply cons () ()) => "(())\n"]
  [(prim-apply cons 1 (prim-apply cons 2 3)) => "(1 2 . 3)\n"]
  [(prim-apply cons 1 (prim-apply cons 2 (prim-apply cons 3 4))) => "(1 2 3 . 4)\n"]
  [(prim-apply car (prim-apply cons 1 2)) => "1\n"]
  [(prim-apply cdr (prim-apply cons 1 2)) => "2\n"]
  [(prim-apply pair? (prim-apply cons 1 2)) => "#t\n"]
  [(prim-apply pair? (prim-apply cdr (prim-apply cons 1 2))) => "#f\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-car! x 99)
     x)
   => "(99 . 2)\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-car! x 99)
     (prim-apply car x))
   => "99\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-car! x 99)
     (prim-apply pair? x))
   => "#t\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-cdr! x 33)
     x)
   => "(1 . 33)\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-cdr! x 33)
     (prim-apply cdr x))
   => "33\n"]
  [(let ([x (prim-apply cons 1 2)])
     (prim-apply set-cdr! x 33)
     (prim-apply pair? x))
   => "#t\n"])

(add-tests-with-string-output "strings"
  [(prim-apply make-string 0) => "\"\"\n"]
  [(prim-apply string? (prim-apply make-string 99)) => "#t\n"]
  [(prim-apply string? 1287) => "#f\n"]
  [(prim-apply string? ()) => "#f\n"]
  [(prim-apply string? #t) => "#f\n"]
  [(prim-apply string? #f) => "#f\n"]
  [(let ([s (prim-apply make-string 1)])
    (prim-apply string-set! s 0 #\z)
    s)
   => "\"z\"\n"]
  [(let ([s (prim-apply make-string 3)])
    (prim-apply string-set! s 0 #\a)
    (prim-apply string-set! s 1 #\b)
    (prim-apply string-set! s 2 #\c)
    s)
   => "\"abc\"\n"]
  [(let ([s (prim-apply make-string 1)])
    (prim-apply string-set! s 0 #\z)
    (prim-apply string-ref s 0))
   => "#\\z\n"]
  [(let ([s (prim-apply make-string 3)])
    (prim-apply string-set! s 0 #\a)
    (prim-apply string-set! s 1 #\b)
    (prim-apply string-set! s 2 #\c)
    (prim-apply string-ref s 2))
   => "#\\c\n"]
  [(let ([s (prim-apply make-string 3)])
    (prim-apply string-length s))
   => "3\n"])

(add-tests-with-string-output "vectors"
  [(prim-apply make-vector 0) => "#()\n"]
  [(prim-apply vector? (prim-apply make-vector 0)) => "#t\n"]
  [(prim-apply vector? 1287) => "#f\n"]
  [(prim-apply vector? ()) => "#f\n"]
  [(prim-apply vector? #t) => "#f\n"]
  [(prim-apply vector? #f) => "#f\n"]
  [(let ([v (prim-apply make-vector 1)])
    (prim-apply vector-set! v 0 999)
    v)
   => "#(999)\n"]
  [(let ([v (prim-apply make-vector 3)])
    (prim-apply vector-set! v 0 999)
    (prim-apply vector-set! v 1 777)
    (prim-apply vector-set! v 2 666)
    v)
   => "#(999 777 666)\n"]
  [(let ([v (prim-apply make-vector 1)])
    (prim-apply vector-set! v 0 999)
    (prim-apply vector-ref v 0))
   => "999\n"]
  [(let ([v (prim-apply make-vector 3)])
    (prim-apply vector-set! v 0 999)
    (prim-apply vector-set! v 1 777)
    (prim-apply vector-set! v 2 666)
    (prim-apply vector-ref v 2))
   => "666\n"]
  [(let ([v (prim-apply make-vector 3)]
         [s (prim-apply make-string 3)])
    (prim-apply string-set! s 0 #\a)
    (prim-apply string-set! s 1 #\b)
    (prim-apply string-set! s 2 #\c)
    (prim-apply vector-set! v 0 (prim-apply cons 1 2))
    (prim-apply vector-set! v 1 (prim-apply cons 3 4))
    (prim-apply vector-set! v 2 s)
    v)
   => "#((1 . 2) (3 . 4) \"abc\")\n"]
  [(let ([v1 (prim-apply make-vector 3)]
         [v2 (prim-apply make-vector 1)]
         [s (prim-apply make-string 3)])
    (prim-apply string-set! s 0 #\a)
    (prim-apply string-set! s 1 #\b)
    (prim-apply string-set! s 2 #\c)
    (prim-apply vector-set! v1 0 (prim-apply cons 1 2))
    (prim-apply vector-set! v1 1 (prim-apply cons 3 4))
    (prim-apply vector-set! v1 2 s)
    (prim-apply vector-set! v2 0 v1)
    v2)
   => "#(#((1 . 2) (3 . 4) \"abc\"))\n"])
